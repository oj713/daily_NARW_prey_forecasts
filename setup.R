suppressPackageStartupMessages(
  {
    library(stars) # spatial data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(purrr)
    library(sf)
  })

### PREDICTION AND PLOT HELPERS
plot_gen <- function(data, plot_col, title = "Plot", size = .3, log_col = FALSE) {
  p <- ggplot(data, aes(x = longitude, y = latitude)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 50), expand = TRUE) +
    theme_bw() + 
    ggtitle(title)
  
  if (log_col) {
    p + 
      geom_point(aes(col = log(get(plot_col) + 1)), alpha = .7, size = size) + 
      labs(col = paste(plot_col, "(log[x + 1])"))
  } else {
    p + 
      geom_point(aes(col = get(plot_col)), alpha = .7, size = size) + 
      labs(col = plot_col)
  }
}

#' Saves a plot object to file
#' @param plot_obj, obj to save to pdf
#' @param filename str, name of file excluding .pdf
#' @param root root folder location
save_pdf_ecocast <- function(plot_obj, filename, root) {
  pdf(file.path(root, paste0(filename, ".pdf")))
  print(plot_obj)
  dev.off()
}

# Returns a list of variable abbreviations
var_abb <- function() {
  list(bathymetry = "Bathymetry", 
       mlotst = "Mixed layer depth", 
       thetao = "Surface temperature", 
       bottomT = "Bottom temperature", 
       sob = "Bottom salinity", 
       so = "Surface salinity", 
       vo = "Northward velocity",
       uo = "Westward velocity",
       zos = "Sea surface height")
}
