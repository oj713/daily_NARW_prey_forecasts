source("setup.R")
jec <- get_input_data("jelly_phys_bgc_bathy_ET_chfc.csv.gz") |>
  mutate(vel = sqrt(uo^2 + vo^2), 
         month = lubridate::month(date) |> factor(levels = 1:12)) |>
  select(-uo, -vo)
vars <- colnames(jec)[-c(1:7)]
fitted_wkf <- readRDS("model_exploration/jelly_test_model_v0.csv.gz")

#' generate response curves for a desired workflow object
#' 
#' @param wkf_obj wkf, workflow
#' @param vdata df, the data used to determine ranges for model
#' @param wkf_title str, title of workflow
#' @param log_bathy bool, convert bathymetry to log scale?
#' @param same_y bool, same y scale for all covariates
#' @return response curves separated by month and variable ordered by imp
response_curves_indmodel <- function(wkf_obj,
                                     vdata,
                                     wkf_title,
                                     log_bathy = TRUE,
                                     same_y = FALSE) {
  
  role_table <- wkf_obj |> extract_preprocessor() |> summary()
  vars <- filter(role_table, role == "predictor")$variable
  ids <- filter(role_table, role == "id")$variable
  
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
  
  # Table summarizing median, 95% range for all covariates
  if (log_bathy) {
    quantiles[,"bathy_depth"] <- log10(quantiles[,"bathy_depth"] + 1)
  }
  quant_table <- quantiles |>
    base::t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%")))
  if(has_month) {
    quant_table <- quant_table |>
      bind_rows(list(variable = "month", `2.5%` = 8, 
                     `50%` = 8, `97.5%` = 8))
  }
  
  # helper that generates evaluation strip and runs predictions
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
    model_preds <- predict(wkf_obj, in_df, type = "prob") |>
      bind_cols(value = pull(in_df, var) |>
                  as.numeric()) |>
      mutate(variable = var)
    
    if(log_bathy && var == "bathy_depth") {
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
  vimp <- (wkf_obj |>
    extract_fit_parsnip() |>
    vi_model())$Variable
  if (has_month) {
    vimp <- gsub("month[^ ]*", "month", vimp) |>
      unique()
  }
  names(vimp) <- vimp
  if (log_bathy) {
    vimp["bathy_depth"] <- "bathy (log 10)"
  }
  
  quant_table <- quant_table |>
    mutate(across(variable, ~vimp[.x] |> unlist())) |>
    mutate(across(variable, ~factor(.x, levels = vimp)))
  
  eval_strip <- eval_strip |>
    mutate(across(variable, ~vimp[.x] |> unlist())) |>
    mutate(across(variable, ~factor(.x, levels = vimp)))
  
  # plotting
  ggplot(quant_table) +
    geom_rect(aes(xmin = `2.5%`, xmax = `97.5%`, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = .12) +
    geom_vline(aes(xintercept = `50%`), color = "red") +
    geom_line(data = eval_strip,
              mapping = aes(x = value, y = .pred_TRUE, linetype = "th")) +
    facet_wrap(~ variable, scales = ifelse(same_y, "free_x", "free")) +
    theme_bw() +
    labs(x = "Covariate value", 
         y = "Predicted probability") +
    ggtitle(paste("Response Curves:", wkf_title)) +
    theme(legend.position = "none")
}


plots <- workflow_list |>
  imap(\(x, idx) response_curves_indmodel(x, jec, idx, 
                                          log_bathy = TRUE, same_y = TRUE))

pdf("model_exploration/representing_seasonality/response_curves_comparisons.pdf")
plots
dev.off()
