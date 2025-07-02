#' generate response curves for a desired version
#' 
#' @param v the version
#' @param data the data used to determine ranges for model
#' @param vars the vars included
#' @param num_pts the resolution of the lines 
#' @param mid_mon the middle month
#' @param log_bathy convert bathymetry to log scale?
#' @param vimp the list of most important variables
#' @param post post-prediction correction function, if applicable
#' @return response curves separated by month and variable 
response_curves_data <- function(model_obj,
                                 vdata,
                                 vars = c("Bathy_depth", "SST", #"SSS", 
                                          "Tbtm", "MLD", "Sbtm", "Vel"),
                                 num_pts = 100,
                                 mid_mon = 8,
                                 log_bathy = TRUE,
                                 show_no_post = TRUE,
                                 same_y = FALSE,
                                 save_plot = TRUE,
                                 patch_only_medians = FALSE,
                                 bottom_latitude = NULL,
                                 vimp = NULL,
                                 post = NULL) {
  
  #vdata <- data |>
    #mutate(Vel = sqrt(U^2 + V^2))
  
  # retrieving workflow
  # wkfs <- get_v_wkfs(v)
  
  vdata <- vdata |>
    mutate(vel = sqrt(uo^2 + vo^2))
  
  vars <- c("so", "thetao", "bottomT", "mlotst", "vel", "bathy_depth")
  
  # acquiring quantile ranges of covariates - max, min, median, 95% range
  quantiles <- vdata |>
    select(all_of(vars)) |>
    apply(2, function(x) quantile(x, probs = c(0, .025, .5, .975, 1)))
  
  # COMMENT OUT IF NOT REPLACING TBTM
  # quantiles[3, 3] <- 4.5
  
  # generating steps for each covariate between max and min
  range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                          ~seq(.x, .y, length.out = num_pts))
  
  # Replacing medians as needed
  if (patch_only_medians) {
    quantiles['50%',] <- vdata |> 
      filter(patch == "1") |>
      select(all_of(vars)) |>
      apply(2, function(x) quantile(x, probs = c(.5)))
  } else if (!is_null(bottom_latitude)) {
    quantiles['50%',] <- vdata |>
      filter(lat > 42) |>
      select(all_of(vars)) |>
      apply(2, function(x) quantile(x, probs = c(.5)))
  }
  
  medians <- quantiles['50%',] |>
    as_tibble_row() |>
    mutate(month = as.factor(mid_mon))
  
  # Table summarizing median, 95% range for all covariates
  if (log_bathy) {
    quantiles[,"bathy_depth"] <- log10(quantiles[,"bathy_depth"] + 1)
  }
  quant_table <- quantiles |>
    base::t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%"))) |>
    bind_rows(list(variable = "month", `2.5%` = mid_mon, 
                   `50%` = mid_mon, `97.5%` = mid_mon))
  
  # helper that generates evaluation strip and runs predictions
  var_response <- function(var, post_func = post) {
    
    base <- bind_cols(lon = NA, 
                      select(medians, -all_of(var)))
    if (!("lat" %in% vars)) {
      base <- bind_cols(lat = NA)
    }
    
    # input dataframe for workflows
    in_df <- (if(var == "month"){
      bind_cols(base, month = 1:12 |> as.factor())
    } else {
      bind_cols(base, select(range_steps, all_of(var)))
    }) |>
      rename(V = Vel) |>
      mutate(U = 0)
    
    # Predicting, binding value
    model_preds <- apply_quantile_preds(wkfs, in_df, c(.025, .5, .975)) |>
      bind_cols(value = pull(in_df, ifelse(var == "Vel", "V", var)) |>
                  as.numeric()) |>
      mutate(variable = var)
    
    if(log_bathy && var == "Bathy_depth") {
      model_preds <- model_preds |>
        mutate(value = log10(value + 1))
    }
    if(show_no_post && !is.null(post)) { #&& var == "Bathy_depth"
      model_preds <- model_preds |>
        mutate(no_post = `50%`)
    }
    
    if (!is.null(post_func)) {
      bathymetry_col <- (if(var == "Bathy_depth") {
        range_steps$Bathy_depth
      } else {
        medians[["Bathy_depth"]]
      })
      
      model_preds <- model_preds |>
        mutate(bathymetry_col = bathymetry_col) |>
        mutate(across(ends_with("%"), 
                      function(x) {x * post_func(bathymetry_col)})) |>
        select(-bathymetry_col)
    }
    
    model_preds
  }
  
  # creating plottable object
  eval_strip <- c(colnames(quantiles), "month") |>
    lapply(var_response) |>
    bind_rows()
  
  if (!is_null(vimp)) {
    vimp <- gsub("month[^ ]*", "month", vimp) |>
      unique()
    vimp <- var_abb()[vimp] |> unlist()
    if (log_bathy) {
      vimp["Bathy_depth"] <- "Bathymetry (log 10)"
    }
    
    quant_table <- quant_table |>
      mutate(across(variable, ~vimp[.x] |> unlist())) |>
      mutate(across(variable, ~factor(.x, levels = vimp)))
    
    eval_strip <- eval_strip |>
      mutate(across(variable, ~vimp[.x] |> unlist())) |>
      mutate(across(variable, ~factor(.x, levels = vimp)))
  }
  
  # Percentiles
  # eval_strip |>
  #   filter(`50%` > .01) |>
  #   mutate(diff = `97.5%`-`2.5%`) |>
  #   group_by(variable) |>
  #   summarize(median_range = median(diff),
  #             median_mean = median(`50%`),
  #             .groups = "keep") |>
  #   mutate(ratio = median_range/median_mean)
  
  # plotting
  plot <- ggplot(quant_table) +
    geom_rect(aes(xmin = `2.5%`, xmax = `97.5%`, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = .12) +
    geom_ribbon(data = eval_strip, 
                aes(x = value, ymax = `97.5%`, ymin = `2.5%`),
                fill = "grey50", alpha = .8) +
    geom_line(data = eval_strip,
              mapping = aes(x = value, y = `50%`, linetype = "th")) +
    facet_wrap(~ variable, scales = ifelse(same_y, "free_x", "free"), 
               nrow = ifelse(save_plot, 3, 2), ncol = ifelse(save_plot, 3, 4)) +
    theme_bw() +
    labs(x = "Covariate value", 
         y = expression("Predicted"~τ[b]*"-patch,"~τ[h]*"-patch probability")) +
    ggtitle(paste(v, 
                  ifelse(is_null(post), "tau-b", "tau-h"), 
                  "Response Curves"))
  
  if (show_no_post && !is.null(post)) {
    plot <- plot + 
      geom_line(data = eval_strip, 
                mapping = aes(x = value, y = no_post, linetype = "tb"),
                color = "blue")
  }
  
  plot <- plot +
    scale_linetype_manual(
      labels = c("th" = expression(τ[h]*"-patch probability"), 
                 "tb" = expression(τ[b]*"-patch probability")),
      values = c("th" = 1, "tb" = 6),
      drop = FALSE
    ) +
    geom_vline(aes(xintercept = `50%`), color = "red") +
    theme(legend.position = "bottom", 
          legend.title = element_blank())
  
  if (save_plot) {
    savename <- paste0("response_curves_mon", mid_mon, 
                       ifelse(!is_null(post), "_corrected", ""),
                       ifelse(show_no_post, "WNoPost", ""),
                       ifelse(log_bathy, "_logbathy", ""),
                       ifelse(same_y, "_fixedY", ""),
                       ifelse(!is_null(bottom_latitude), paste0("_lat", bottom_latitude), ""))
    save_analysis(plot, v, savename)
  } 
  
  return (plot)
}