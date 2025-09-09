species <- "jellyfish"
source("setup.R")
library(corrplot)
library(lubridate)

data <- get_input_data(paste0(species, "_copernicus_matched.csv.gz"))
ignore_vars <- c("lon", "lat", "date", "mask")
var_cols <- colnames(data)[!colnames(data) %in% ignore_vars]

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
      ggtitle(paste0("Matched ", species, "/Copernicus: ", var))
  }
  
  col_names |>
    map(plot_variable) |> 
    save_eda_ecocast(filename)
  
  TRUE
}

#' Retrieves correlation matrix and saves to file
#' @param data df, covariate dataset. Required columns lon, lat
#' @param cols numeric or char, columns to plot
#' @param filename str, filename minus .pdf ending
#' @return plot
get_correlation_matrix <- function(data, cols, filename = "variable_corrs") {
  correlation_matrix <- data[cols] |>
    cor()
  
  corrplot(correlation_matrix, addCoef.col = "black", number.cex = .6,
           method = "circle", type = "full", order = "alphabet") |>
    save_eda_ecocast(filename)
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
    save_eda_ecocast(filename)
}

#' Plots ecomon data in July
explore_july <- function() {
  ec <- ecomon::scale_ecomon() |> mutate(month = month(date))
  
  julyhistogram <- ggplot(filter(ec, month == 7), aes(x = date)) + 
    geom_histogram(fill = "darkorange") + 
    theme_bw() + 
    labs(x = "Year", y = "Num records (line at 1993)", title= "Ecomon records in July over time") +
    geom_vline(aes(xintercept = as.Date("1993-01-01")))
  
  ecjd_targets <- ec |>
    filter(month == 7, year(date) > 1993) |>
    select(lat, lon, date, coel_m2, salps_m2, 
           calfin_m2, pseudo_m2, siph_m2, ctyp_m2)
  
  julyplot <- plot_gen(ecjd_targets[complete.cases(ecjd_targets),], "date", size = 1,
                       title = "Ecomon July datapoints for target species")
  
  list(julyhistogram, julyplot) |>
    save_eda_ecocast("july_explorations")
}



