source("io_stars.R")

#' Helper: saves a plot object to file
save_qs_plot <- function(plottable_obj, v, filename, 
                         pred_folder = "daily_resolution") {
  pdf(v_path(v, "preds", pred_folder, paste0(filename, ".pdf")))
  print(plottable_obj)
  dev.off()
  
  plottable_obj
}

#' Helper: Plots a quantile stars object
plot_qs_general <- function(quantile_stars, v,
                            viridis_option = "viridis", 
                            fill_title = "Presence probability",
                            title = "Title") {
  ggplot() + 
    geom_stars(data = quantile_stars) + 
    scale_fill_viridis(na.value = "transparent", option = viridis_option) +
    coord_quickmap() + 
    theme_bw() +
    theme(legend.position = "bottom") + 
    labs(x = "Longitude", y = "Latitude", fill = fill_title, 
         title = paste(str_to_title(v), title)) +
    theme(strip.background = element_blank(), strip.placement = "outside")
}

#' Plots each month 
plot_monthly_averages <- function(v) {
  if (!file.exists(v_pred_path(v, "monthly"))) {
    stop("Consolidated monthly file must exist.")
  }
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,], 
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$uncertainty <- m_agg$`95%` - m_agg$`5%`
  
  plot_50 <- m_agg["50%",,,] |>
    plot_qs_general(v, 
                    viridis_option = "viridis", 
                    fill_title = "Presence probability",
                    title = "averaged monthly probability (1993 - 2019)") +
    facet_wrap(~factor(geometry, labels = month.abb))
  
  plot_uncertainty <- m_agg["uncertainty",,,] |>
    plot_qs_general(v, 
                    viridis_option = "inferno", 
                    fill_title = "90% uncertainty range",
                    title = "averaged monthly uncertainty (1993 - 2019)") +
    facet_wrap(~factor(geometry, labels = month.abb))
  
  rm(ym_stars, m_agg)
  gc()
  
  save_qs_plot(list(plot_50, plot_uncertainty), v, 
               paste0(species, "_plot_monthly_averages"))
}

## broken at present
plot_individual_dates <- function(v, dates,
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
    ystars <- ystars[cols_to_retrieve,,,lubridate::yday(dates[d_years == year])]
  }
  dates_stars <- unique(d_years) |> map(get_data_year)
  gc()
  dates_stars <- do.call(c, c(dates_stars, along = "date"))
  if ("uncertainty" %in% cols_to_plot) {
    dates_stars$uncertainty <- dates_stars$`95%` - dates_stars$`5%`
  }
  
  if (verbose) {"\rPlotting...           "}
  
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
      do.call(plot_qs_general, append(list(dates_stars[col_to_plot,,,], v), qs_options)) + 
        facet_wrap(~date)
    } else {
      st_get_dimension_values(dates_stars, "date") |>
        imap(function(date, idx) {
          plot_qs_general(dates_stars[col_to_plot,,,idx], v, qs_options[[1]],
                          qs_options[[2]], paste0(qs_options[[3]], ": ", date))
        })
    })
    
    # Saving to file
    save_qs_plot(plots_to_save, v, 
      paste(c(species, date_range_to_string(dates), 
              gsub("%", "", col_to_plot), ifelse(gridded, "gridded", "paneled")), collapse = "_"))
  }
  
  walk(cols_to_plot, plot_col)
  
  TRUE
}

gif_date_range <- function(v, 
                           date_start, 
                           date_end,
                           by = "days") {
  
}

