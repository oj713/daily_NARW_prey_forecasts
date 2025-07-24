species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")

v <- "je.0.00"
wkfs <- get_v_wkfs(v)
testing <- get_v_testing(v)
vdata <- v |> read_config() |> data_from_config()

#' Saves and returns a plot object
#' @param v str, version
#' @param plot_obj ggplot, plot object
#' @param filename str, filename to save to sans .pdf extension
#' @param save bool, save to file or just return object? 
save_and_return <- function(v, plot_obj, filename, save = TRUE) {
  if (save) {
    pdf(v_path(v, "model", paste0(filename, ".pdf")))
    print(plot_obj)
    dev.off()
  }
  plot_obj
}

#' Plots threshold vs. performance as assessed by F1 score
#' @param v str, version
#' @param testing df, testing results
#' @param save bool, save to file? 
threshold_vs_performance <- function(v, testing, save = TRUE) {
  # Calculate sensitivity, specificity, and f1 score for a threshold
  calc_metrics <- function(threshold) {
    threshold_df <- testing |>
      transmute(wkf_id, patch, 
                estimate = (.pred_1 > threshold) |> as.numeric() |> 
                    factor(levels = c("1", "0")))
    
    metrics <- metric_set(sens, spec, f_meas)
    
    threshold_df |>
      group_by(wkf_id) |>
      group_map(~{ # Calculate metrics across folds
        suppressWarnings(metrics(.x, truth = patch, estimate = estimate) |> 
                           mutate(.y))}) |>
      bind_rows() |>
      group_by(.metric) |>
      group_map(~{ # Compress down to quantile results, deliberate catch for NA values
        if (any(is.na(.x$.estimate))) {
          c('5%' = NA, '50%' = NA, '95%' = NA, .y)
        } else {
          c(quantile(.x$.estimate, probs = c(.05, .5, .95)), .y)
        }}) |>
      bind_rows() |>
      mutate(threshold = threshold)
  }
  
  res <- seq(0, 1, length.out = 100) |>
    map(calc_metrics) |>
    bind_rows()
  res <- res[complete.cases(res),]
  f_meas_res <- filter(res, .metric == "f_meas")
  best_threshold <- f_meas_res$threshold[[which.max(f_meas_res$`50%`)]]
  
  plot <- ggplot(res, aes(x = threshold)) + 
    geom_vline(aes(xintercept = best_threshold), col = "grey40") +
    geom_ribbon(aes(ymin = `5%`, ymax = `95%`, fill = .metric), alpha = .3) + 
    geom_line(aes(col = .metric, y = `50%`)) + 
    theme_bw() + 
    labs(x = "Threshold", col = "Metric", fill = "Metric", y = "Value",
         title = paste0(v, " threshold vs. performance (Best: ", 
                        round(best_threshold, 2), ")"))
  
  save_and_return(v, plot, "threshold_vs_performance", save)
}

#' Calculates response curves
#' 
#' @param v str, version
#' @param v_wkfs workflow set
#' @param vdata df, the data used to determine ranges for model
#' @param same_y bool, same y scale for all covariates
#' @param save bool, save to file?
#' @return response curves separated by month and variable ordered by imp
response_curves_data <- function(v, 
                                 v_wkfs,
                                 vdata,
                                 same_y = FALSE, 
                                 save = TRUE) {
  # Extracting variables to process from workflows
  role_table <- v_wkfs[[1]] |> extract_preprocessor() |> summary()
  vars <- filter(role_table, role == "predictor")$variable
  ids <- filter(role_table, role == "ID")$variable
  
  has_month = FALSE
  if ("month" %in% vars) {
    has_month = TRUE
    vars <- vars[!vars == "month"]
  }
  
  # acquiring quantile ranges of covariates - max, min, median, 95% range
  quantiles <- vdata |>
    select(all_of(vars)) |>
    apply(2, function(x) quantile(x, probs = c(0, .025, .5, .975, 1)))
  
  # generating steps for each covariate between max and min
  range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                          ~seq(.x, .y, length.out = 100))
  
  medians <- quantiles['50%',] |>
    as_tibble_row()
  if(has_month) {
    medians <- medians |> mutate(month = 8 |> as.factor())
  }
  quantiles[,"bathy_depth"] <- log10(quantiles[,"bathy_depth"] + 1)
  
  # Table summarizing median, 95% range for all covariates
  quant_table <- quantiles |>
    base::t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%")))
  if(has_month) {
    quant_table <- quant_table |>
      bind_rows(list(variable = "month", `2.5%` = 8, 
                     `50%` = 8, `97.5%` = 8))
  }
  
  #' Generates evaluation strip for a single variable
  #' @param var str, variable to evaluate
  #' @return df with variable value and model response, all other variables static
  var_response <- function(var) {
    
    base <- select(medians, -all_of(var)) |>
      mutate(!!!setNames(rep(NA, length(ids)), ids))
    
    # input dataframe for workflows
    in_df <- (if(var == "month"){
      bind_cols(base, month = 1:12 |> as.factor())
    } else {
      bind_cols(base, select(range_steps, all_of(var)))
    })
    
    # Predicting, binding value
    model_preds <- apply_quantile_preds(wkfs, in_df, c(.025, .5, .975)) |>
      bind_cols(value = pull(in_df, var) |> as.numeric()) |>
      mutate(variable = var)
    
    if(var == "bathy_depth") {
      model_preds <- model_preds |>
        mutate(value = log10(value + 1))
    }
    
    model_preds
  }
  
  # creating plottable object
  eval_strip <- colnames(medians) |>
    lapply(var_response) |>
    bind_rows()
  
  ### Variable importance
  vimp <- (wkfs[[1]] |>
             extract_fit_parsnip() |>
             vi_model())$Variable
  if (has_month) {
    vimp <- gsub("month[^ ]*", "month", vimp) |>
      unique()
  }
  names(vimp) <- vimp
  vimp["bathy_depth"] <- "bathy (log 10)"
  
  quant_table <- quant_table |>
    mutate(across(variable, ~vimp[.x] |> unlist())) |>
    mutate(across(variable, ~factor(.x, levels = vimp)))
  
  eval_strip <- eval_strip |>
    mutate(across(variable, ~vimp[.x] |> unlist())) |>
    mutate(across(variable, ~factor(.x, levels = vimp)))
  
  # plotting
  plot <- ggplot(quant_table) +
    geom_rect(aes(xmin = `2.5%`, xmax = `97.5%`, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = .12) +
    geom_vline(aes(xintercept = `50%`), color = "red") +
    geom_ribbon(data = eval_strip, 
                aes(x = value, ymax = `97.5%`, ymin = `2.5%`),
                fill = "grey50", alpha = .8) +
    geom_line(data = eval_strip,
              mapping = aes(x = value, y = `50%`)) +
    facet_wrap(~ variable, scales = ifelse(same_y, "free_x", "free")) +
    theme_bw() +
    labs(x = "Covariate value", 
         y = "Predicted Probability") +
    ggtitle(paste(v, "response curves"))
  
  savename <- paste0("response_curves", 
                     ifelse(same_y, "_fixedY", ""))
  save_and_return(v, plot, savename, save)
}

