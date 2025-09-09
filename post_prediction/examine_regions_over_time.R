source("data_preparation/data_from_config.R")
source("io_stars.R")

#' Plots the daily forecasts shapefile
plot_region_polygons <- function() {
  ## Retrieve region polygons
  regions <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  ordered_reg <- c("Lower MAB", "Upper MAB", "GoM", "GB", "WSS", "ESS", "swGSL", "nGSL", "NLS")
  
  # Plot region polygons
  ggplot(regions) +
    geom_sf(aes(fill = factor(id, levels = ordered_reg)), 
            col = "black", alpha=.3, show.legend = FALSE) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    geom_sf_label(aes(label = id), cex = 3) +
    coord_sf(xlim = c(-78, -40), ylim = c(35, 55), expand = TRUE) +
    theme_bw() + 
    ggtitle("Northwest Atlantic: Regions") + 
    labs(x = "lon", y = "lat", fill = "Region")
}

#' Plots observed vs. predicted over month and year, aggregated over regions
#' Consolidated monthly predictions MUST exist already!
#' @param v str, version
#' @return plots, also saves to file
observed_vs_predicted_regional <- function(v) {
  if (!file.exists(v_pred_path(v, "monthly"))) {
    stop("Must have called consolidate_preds_monthly() to generate averages!")
  }
  
  ## Retrieve region polygons
  regions <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  ordered_reg <- c("Lower MAB", "Upper MAB", "GoM", "GB", "WSS", "ESS", "swGSL", "nGSL", "NLS")
  
  ## Retrieving base datasets for both predicted and observed
  observed_sf <- data_from_config(read_config(v)) |>
    st_as_sf(crs = 4326, coords = c("lon", "lat")) |>
    st_join(regions) |>
    na.omit() |>
    mutate(month = lubridate::month(date), year = lubridate::year(date),
           patch = patch |> as.character() |> as.numeric())
  predicted_sf <- read_quantile_stars(v_pred_path(v, "monthly")) |>
    aggregate(by = regions, FUN = mean, na.rm = TRUE) |>
    as.data.frame(long = TRUE) |>
    mutate(month = lubridate::month(time), year = lubridate::year(time))
  
  #' Helper: generates plot for a time type defined in obs/pred sf
  plot_time_type <- function(time_type) {
    # Group both datasets by time type (months or years)
    observed_by_time <- observed_sf |>
      group_by("tu" = get(time_type), id) |>
      summarize(meanpatch = mean(patch), numobs = n(), .groups = "drop_last")
    predicted_by_time <- predicted_sf |>
      group_by("tu" = get(time_type), geometry) |>
      summarize(mean50 = mean(`50%`), mean5 = mean(`5%`), mean95 = mean(`95%`),
                .groups = "drop_last") |>
      left_join(regions) |> dplyr::select(-geometry)
    
    # Plot
    ggplot(observed_by_time, mapping = aes(x = tu)) +
      geom_line(aes(y = meanpatch), col = "palevioletred2") + 
      geom_point(aes(y = meanpatch, size = numobs),  col = "palevioletred2") +
      geom_ribbon(data = predicted_by_time, aes(ymin = mean5, ymax = mean95), alpha = .2) +
      geom_line(data = predicted_by_time, aes(y = mean50)) +
      facet_wrap(~id) +
      theme_bw() + 
      theme(legend.position = "bottom") + 
      labs(x = str_to_title(time_type), 
           y = "Average observed (pink) / predicted (black) patch", size = "No. observations", 
           title = paste(str_to_title(v), "observed vs. predicted over", time_type))
  }
  
  plots <- list("month", "year") |> lapply(plot_time_type)
  
  pdf(v_path(v, "model", "observed_vs_predicted_regional.pdf"))
  print(plots)
  dev.off()
  
  plots
}
