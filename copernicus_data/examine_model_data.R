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
                                 "mec_brickman_bathy.csv.gz"))
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

