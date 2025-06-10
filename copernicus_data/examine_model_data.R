library(ggplot2)
library(viridis)
library(dplyr)
library(purrr)


root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

#' Saves exploratory data analysis pdf to file, wrapper of save_pdf_ecocast
save_eda <- function(obj, filename) {
  save_pdf_ecocast(obj, filename, root)
  obj
}

mec <- readr::read_csv(file.path(root, 
                                 "mec_brickman_bathy.csv.gz"), 
                       show_col_types = FALSE)
nrow(mec)

#' Plots each variable on a map
#' Saves result to file
get_variable_plots <- function() {
  #' Plots a variable
  plot_variable <- function(var) {
    ggplot(mec, aes(x = longitude, y = latitude)) + 
      coord_quickmap() + 
      theme_bw() + 
      scale_color_viridis() +
      geom_point(aes(col = get(var)), cex = .4, alpha = .5) + 
      labs(col = var, x = "Longitude", y = "Latitude") + 
      ggtitle(paste("Matched Ecomon/Copernicus:", var))
  }
  
  c("corrected_CIV_CVI_m2", colnames(mec)[7:14]) |>
    map(plot_variable) |> 
    save_eda("variable_plots")
}

library(corrplot)

#' Retrieves correlation matrix and saves to file
get_correlation_matrix <- function() {
  correlation_matrix <- mec |>
    select(bottomT:bathymetry) |>
    cor()
  
  corrplot(correlation_matrix, addCoef.col = "black", 
           method = "circle", type = "full", order = "alphabet") |>
    save_eda("variable_correlations")
}

#' Create abundance vs. variable value scatterplots
library(lubridate)

get_abundance_variable <- function(var_name) {
  mec_logab <- mec |>
    mutate(logabund = log(corrected_CIV_CVI_m2 + 1),
           month = month(date) |> factor(levels = 1:12))
  
  r2_overall <- cor(mec_logab$logabund, pull(mec_logab, var_name))^2 |>
    round(4)
  
  monthly_labels <- mec_logab |>
    group_by(month) |>
    summarize(r2 = cor(logabund, get(var_name))^2 |> round(4)) |>
    apply(1, function(row) {
      paste0(month.abb[[as.integer(row[[1]])]], ", R²: ", row[[2]])
      #bquote(.(month.abb[[as.integer(row[[1]])]]) ~ "," ~ R^2 * ":" ~ .(row[[2]]))
    })  
  names(monthly_labels) <- levels(mec_logab$month)
  
  ggplot(mec_logab, aes(x = logabund, y = get(var_name))) + 
    geom_point(alpha = .2) + 
    theme_bw() +
    geom_smooth(method = "lm") + 
    facet_wrap(~ month, labeller = labeller(month = monthly_labels)) +
    labs(x = "Abundance (log[x + 1])", y = var_name, 
         title = bquote(.(var_name) ~ "Correlations:" ~ R^2: ~ .(r2_overall)))
}

colnames(mec)[7:14] |>
  map(get_abundance_variable) |>
  save_eda("abundance_vs_covariates_monthly")





