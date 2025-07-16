source("setup.R")
library(corrplot)
library(lubridate)

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data"
#' Saves exploratory data analysis pdf to file, wrapper of save_pdf_ecocast
save_eda <- function(obj, filename) {
  save_pdf_ecocast(obj, filename, root)
  obj
}

cfin_data <- get_input_data("mec_bathy_BR.csv.gz")
jellydata <- get_input_data("jelly_phys_bgc_bathy_ET_chfc.csv.gz")

#' Plots each variable on a map and saves result to file
#' @param data df, covariate dataset. Required columns lon, lat
#' @param col_names char, vector of data column names to plot
#' @param filename str, filename minus .pdf ending
#' @return plots
get_variable_plots <- function(data, col_names, filename = "variable_plots") {
  #' Plots a variable
  plot_variable <- function(var) {
    ggplot(data, aes(x = lon, y = lat)) + 
      coord_quickmap() + 
      theme_bw() + 
      scale_color_viridis() +
      geom_point(aes(col = get(var)), cex = .4, alpha = .5) + 
      labs(col = var, x = "Longitude", y = "Latitude") + 
      ggtitle(paste("Matched Ecomon/Copernicus:", var))
  }
  
  col_names |>
    map(plot_variable) |> 
    save_eda(filename)
  
  TRUE
}

#' Retrieves correlation matrix and saves to file
#' @param data df, covariate dataset. Required columns lon, lat
#' @param col_indices numeric, vector of data column indices to plot
#' @param filename str, filename minus .pdf ending
#' @return plot
get_correlation_matrix <- function(data, col_indices, filename = "variable_corrs") {
  correlation_matrix <- data[col_indices] |>
    cor()
  
  corrplot(correlation_matrix, addCoef.col = "black", 
           method = "circle", type = "full", order = "alphabet") |>
    save_eda(filename)
}

#' Create abundance vs. variable value scatterplots
#' @param data df, covariate dataset. Required columns ind_m2, date
#' @param col_names char, vector of data column names to plot
#' @param filename str, filename minus .pdf ending
#' @return plots
get_abund_vs_var_plots <- function(data, col_names, filename = "var_vs_abund") {
  
  # Helper to plot a single variable
  get_abundance_variable_plot <- function(var_name) {
    data_logabund <- data |>
      mutate(logabund = log(ind_m2 + 1),
             month = month(date) |> factor(levels = 1:12))
    
    r2_overall <- cor(data_logabund$logabund, pull(data_logabund, var_name))^2 |>
      round(4)
    
    monthly_labels <- data_logabund |>
      group_by(month) |>
      summarize(r2 = cor(logabund, get(var_name))^2 |> round(4)) |>
      apply(1, function(row) {
        paste0(month.abb[[as.integer(row[[1]])]], ", R²: ", row[[2]])
      })  
    names(monthly_labels) <- levels(data_logabund$month)
    
    ggplot(data_logabund, aes(x = logabund, y = get(var_name))) + 
      geom_point(alpha = .2) + 
      theme_bw() +
      geom_smooth(method = "lm") + 
      facet_wrap(~ month, labeller = labeller(month = monthly_labels)) +
      labs(x = "Abundance (log[x + 1])", y = var_name, 
           title = bquote(.(var_name) ~ "Correlations:" ~ R^2: ~ .(r2_overall)))
  }
  
  col_names |>
    map(get_abundance_variable_plot) |>
    save_eda(filename)
}





