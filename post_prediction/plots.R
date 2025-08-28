source("io_stars.R")

save_qs_plot <- function(plottable_obj, 
                         v, filename, 
                         pred_folder = "daily_resolution") {
  pdf(v_path(v, "preds", pred_folder, filename))
  plottable_obj
  dev.off()
  
  plottable_obj
}

plot_qs_general <- function(quantile_stars, v,
                            viridis_option = "viridis", 
                            fill_title = "Presence probability",
                            title = "Title") {
  ggplot() + 
    geom_stars(data = quantile_stars) + 
    facet_wrap(~factor(geometry, labels = month.abb)) + 
    scale_fill_viridis(na.value = "transparent", option = viridis_option) +
    coord_quickmap() + 
    theme_bw() +
    theme(legend.position = "bottom") + 
    labs(x = "Longitude", y = "Latitude", fill = fill_title, title = title)
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
                    title = paste(str_to_title(v), "averaged monthly probability (1993 - 2019)"))
  
  plot_uncertainty <- m_agg["uncertainty",,,] |>
    plot_qs_general(v, 
                    viridis_option = "inferno", 
                    fill_title = "90% uncertainty range",
                    title = paste(str_to_title(v), "averaged monthly uncertainty (1993 - 2019)"))
  
  rm(ym_stars, m_agg)
  gc()
  
  save_qs_plot(list(plot_50, plot_uncertainty), v, 
               paste0(species, "_plot_monthly_averages.pdf"))
}

plot_individual_dates <- function(v, 
                                  gridded = FALSE) {
  
  
}

gif_date_range <- function(v, 
                           date_start, 
                           date_end,
                           by = "days") {
  
}

