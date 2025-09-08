source("io_stars.R")

###### HELPERS

#' Helper: saves a plot object to file
qshelper_save_plot <- function(plottable_obj, v, filename, 
                         pred_folder = "daily_resolution") {
  pdf(v_path(v, "preds", pred_folder, paste0(filename, ".pdf")))
  print(plottable_obj)
  dev.off()
  
  plottable_obj
}

#' Helper: Plots an individual quantile stars object
qshelper_plot_individual <- function(quantile_stars, v,
                            viridis_option = "viridis", 
                            fill_title = "Presence probability",
                            title = "Title") {
  ggplot() + 
    geom_stars(data = quantile_stars) + 
    scale_fill_viridis(na.value = "transparent", option = viridis_option,
                       limits = c(0, 1)) +
    coord_quickmap() + 
    theme_bw() +
    theme(legend.position = "bottom") + 
    labs(x = "Longitude", y = "Latitude", fill = fill_title, 
         title = paste(str_to_title(v), title)) +
    theme(strip.background = element_blank(), strip.placement = "outside")
}

#' Helper: plots a list of dates and returns plot objects named by savename
qshelper_plot_list <- function(v, dates, 
                               cols_to_plot = c("50%", "uncertainty"),
                               gridded = FALSE, verbose = FALSE) {
  if (verbose) {cat("Retrieving data...")}
  
  # Deriving meta information for stars retrieval
  dates <- sort(dates)
  d_years <- lubridate::year(dates)
  cols_to_retrieve <- cols_to_plot
  if ("uncertainty" %in% cols_to_plot) {
    cols_to_retrieve <- c(cols_to_plot[cols_to_plot != "uncertainty"], "5%", "95%")
  }
  
  # Retrieve stars data for all selected dates/cols
  get_data_year <- function(year) {
    ystars <- read_quantile_stars(v_pred_path(v, year))
    ystars[cols_to_retrieve,,,lubridate::yday(dates[d_years == year])]
  }
  years_to_retrieve <- unique(d_years)
  dates_stars <- (if(length(years_to_retrieve) == 1) {
    get_data_year(years_to_retrieve)
  } else {
    dates_stars_years <- unique(d_years) |> map(get_data_year)
    do.call(c, c(dates_stars_years, along = "date"))
  })
  gc()
  if ("uncertainty" %in% cols_to_plot) {
    dates_stars$uncertainty <- dates_stars$`95%` - dates_stars$`5%`
  }
  
  if (verbose) {cat("\rPlotting...           ")}
  
  # Plotting for each requested column
  plot_col <- function(col_to_plot) {
    # Defining options for text based on uncertainty/not uncertainty
    qs_options <- (
      if(col_to_plot == "uncertainty") {
        list("inferno", "90% uncertainty range", "uncertainty")
      } else {
        list("viridis", paste(col_to_plot, "patch probability"), "probability")
      })
    
    # Different calls depending on whether we're gridding or not
    plots_to_save <- (if (gridded) {
      do.call(qshelper_plot_individual, append(list(dates_stars[col_to_plot,,,], v), qs_options)) + 
        facet_wrap(~date)
    } else {
      st_get_dimension_values(dates_stars, "date") |>
        imap(function(date, idx) {
          qshelper_plot_individual(dates_stars[col_to_plot,,,idx], v, qs_options[[1]],
                          qs_options[[2]], paste0(qs_options[[3]], ": ", date))
        })
    })
    
    # Returning as named list entry
    plot_name <- 
      paste(c(species, date_range_to_string(dates), gsub("%", "perc", col_to_plot), 
              ifelse(gridded, "gridded", "paneled")), collapse = "_")
    
    setNames(list(plots_to_save), plot_name)
  }
  
  res <- lapply(cols_to_plot, plot_col)
  do.call(c, res)
}

###### MAIN FUNCTIONS

#' Plots averaged monthly values of a version across all years
#' Assumes existence of monthly consolidated preds
#' @param v str, version
#' @return plot objects, also saved to file 
plot_monthly_averages <- function(v) {
  if (!file.exists(v_pred_path(v, "monthly"))) {
    stop("Consolidated monthly file must exist.")
  }
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,], 
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$uncertainty <- m_agg$`95%` - m_agg$`5%`
  
  plot_50 <- m_agg["50%",,,] |>
    qshelper_plot_individual(v, 
                    viridis_option = "viridis", 
                    fill_title = "Presence probability",
                    title = "averaged monthly probability (1993 - 2019)") +
    facet_wrap(~factor(geometry, labels = month.abb))
  
  plot_uncertainty <- m_agg["uncertainty",,,] |>
    qshelper_plot_individual(v, 
                    viridis_option = "inferno", 
                    fill_title = "90% uncertainty range",
                    title = "averaged monthly uncertainty (1993 - 2019)") +
    facet_wrap(~factor(geometry, labels = month.abb))
  
  rm(ym_stars, m_agg)
  gc()
  
  qshelper_save_plot(list(plot_50, plot_uncertainty), v, 
               paste0(species, "_plot_monthly_averages"))
}

#' Plots a set of dates and saves to file
#' Each specified column is saved as a separate file
#' @param v str, version
#' @param dates Dates, vector of dates to plot
#' @cols_to_plot str, columns to plot and/or uncertainty
#' @param gridded bool, grid results or have 1 per page?
#' @param verbose bool, show progress?
#' @return TRUE if successful. Saves plot objects to file.
plot_individual_dates <- function(v, dates,
                                  cols_to_plot = c("50%", "uncertainty"),
                                  gridded = FALSE, verbose = FALSE) {
  
  plots_to_save <- qshelper_plot_list(v, dates, cols_to_plot, gridded, verbose)
  
  iwalk(plots_to_save, ~save_qs_plot(.x, v, .y))
  
  TRUE
}

#' GIFs a date set and saves to file
#' @param v str, version
#' @param dates Dates, vector of dates to gif
#' @param cols_to_plot str, columns to gif and/or uncertainty
#' @param delay int, delay in 100ths of a second between frames
#' @param verbose bool, show progress?
#' @return TRUE if successful
plot_dates_gif <- function(v, dates, 
                           cols_to_plot = "50%", delay = 25,
                           verbose = FALSE) {
  plotsets_to_gif <- qshelper_plot_list(v, dates, cols_to_plot, gridded = FALSE, verbose)
  
  # Saving pngs to a temporary directory
  frame_dir <- tempdir()
  png_path <- file.path(frame_dir, "frame%03d.png")
  
  gif_plotset <- function(plotset_to_gif, plotset_name) {
    if (verbose) {cat("\rConverting to GIF:", plotset_name)}
    save_file <- v_path(v, "preds", "daily_resolution", 
                        paste0(gsub("paneled", "gif", plotset_name), ".gif"))
    
    # Save ggplot images as PNGs in tmp directory
    for (i in seq_along(plotset_to_gif)) {
      ggsave(sprintf(png_path, i), plot = plotset_to_gif[[i]], 
             width = 6, height = 5, dpi = 150)
    }
    
    # Retrieve a list of every frame file in the tmp directory
    frame_files <- Sys.glob(file.path(frame_dir, "frame*.png"))
    # Collapse the list into a single string separated by spaces
    frame_list <- paste(shQuote(frame_files), collapse = " ")
    
    # Using convert provided by ImageMagick CLI tools to combine pngs into gif
    convert_command <- sprintf("convert -delay %d -loop 0 %s %s", 
                               delay, frame_list, shQuote(save_file))
    system(convert_command)
    
    file.remove(frame_files)
  }
  
  iwalk(plotsets_to_gif, gif_plotset)
  
  TRUE
}

