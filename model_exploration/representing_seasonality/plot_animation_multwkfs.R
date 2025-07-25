source("setup.R")
library(suncalc)

# Specify dates for animation
dates <- seq(as.Date("2015/4/15"), by = "day", length.out = 70)
# every other day!
dates <- dates[seq(1,length(dates),2)]

dates <- as.Date(c(
  "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20", "2015/4/20",
  "2015/5/20", "2015/6/20", "2015/7/20", "2015/8/20",
  "2015/9/20", "2015/10/20", "2015/11/20", "2015/3/15"
))
dates <- as.Date(c("2015/3/15"))

workflow_list <- NULL

#' Generates a data cube of prediction values over space and time
#' @param dates Date, vector of dates to include
#' @return tibble of phys, bgc, bathy variables for dates
generate_base_cube <- function(dates) {
  ## Copernicus data - physical & bathymetry
  coper_path_phys <- copernicus_path("chfc/GLOBAL_MULTIYEAR_PHY_001_030")
  coper_phys <- coper_path_phys |> read_database() |>
    filter(date %in% dates) |>
    read_andreas(coper_path_phys)
  coper_phys <- correct_andreas(coper_phys, replacement_values = list("mlotst" = 700))
  coper_bathy <- read_static(name = "deptho", path = coper_path_phys)
  ## Copernicus data - biogeochemical
  coper_path_bgc <- copernicus_path("world/GLOBAL_MULTIYEAR_BGC_001_029")
  coper_bgc <- coper_path_bgc |> read_database() |>
    filter(date %in% dates) |>
    read_andreas(coper_path_bgc)
  # Warping to match physical dataset
  coper_bgc_resampled <- st_warp(coper_bgc, dest = coper_phys, method = "near")
  
  # Combining
  coper_data <- c(coper_phys, coper_bgc_resampled)
  coper_data$bathy_depth <- coper_bathy
  rm(coper_phys, coper_bgc, coper_bgc_resampled, coper_bathy)
  gc()
  
  # Changing to predictable tibble format
  coper_return <- as_tibble(coper_data) |>
    mutate(vel = sqrt(uo^2 + vo^2)) |>
    na.omit() |>
    rename(lat = y, lon = x)
  
  if (length(dates) > 1) {
    coper_return <- coper_return |>
      mutate(across(time, as.Date)) |>
      rename(date = time)
  } else {
    coper_return <- coper_return |>
      mutate(date = dates[[1]], .after = lat)
  }
  
  coper_return
}

# Must run seasonality_experiments.Rmd for this to work
seasonality_alterations <- function(coper_data) {
  coper_data_predictable <- 
    bind_cols(coper_data, suncalc::getSunlightPosition(data = coper_data) |>
                select(sun_altitude = altitude, sun_azimuth = azimuth))
  coper_data_predictable <- coper_data_predictable |>
    mutate(day_of_year = lubridate::yday(date), 
           day_length = get_daylength(day_of_year, lat), 
           ddx_day_length = get_ddx_daylength(day_of_year, lat))
  rolling_cols <- season_axes[c("spring", "summer")] |>
    lapply(function(x) percent_month(coper_data_predictable$day_of_year, x)) |>
    bind_cols() |>
    rename(percent_spring = spring, percent_summer = summer)
  coper_data_predictable <- bind_cols(coper_data_predictable, rolling_cols)
  
  coper_data_predictable |>
    mutate(month = lubridate::month(date) |> factor(levels = 1:12),
           depth = NA, 
           ind_m2 = NA)
}  

#### Code to create static image sets across workflows
coper_data_predictable <- fix_date |>
  generate_base_cube() |>
  seasonality_alterations()

bound_preds <- workflow_list |>
  imap(\(x, idx) augment(x, coper_data_predictable, type = "prob") |>
         dplyr::select(lon, lat, date, .pred_TRUE) |>
         mutate(id = idx)) |>
  bind_rows()

date_groups <- bound_preds |> group_by(date) |> group_split()

plot_date <- function(date_group) {
  ggplot(date_group, aes(x = lon, y = lat, fill = .pred_TRUE)) + 
    geom_raster() + 
    coord_quickmap() +
    facet_wrap(~id) + 
    scale_fill_viridis(option = "turbo", limits = c(0, 1)) +
    labs(title = date_group$date[[1]], 
         fill = "Presence Probability", y = "Latitude", x = "Longitude") +
    theme_bw() + 
    theme(legend.position = "bottom")
}

pdf("model_exploration/representing_seasonality/season_comparisons2.pdf")

date_groups |> map(plot_date)

dev.off()


###### Code to create animations
workflow_savenames <- NULL

coper_data_predictable <- dates |>
  generate_base_cube() |>
  seasonality_alterations()

frame_dir <- tempdir()
png_path <- file.path(frame_dir, "frame%03d.png")

#' Generates an animated gif of prediction values over time and saves to file.
#' Works by leveraging ImageMagick CLI, inbuilt software, to coalesce a list of 
#' png files saved to a temp directory into an animated gif. 
#' @param data_cube tibble of all data, required cols .pred_TRUE, x, y, date
#' @param output_file str, filename for output 
#' @return TRUE assuming nothing broke ~ using a try catch is todo
data_cube_gif <- function(data_cube, starting_i = 0) {
  # For each date in the data cube, select all data for that date and plot it
  dates <- unique(data_cube$date)
  for (i in seq_along(dates)) {
    d <- dates[i]
    plotd <- data_cube |> filter(date == d)
    
    p <- ggplot(plotd, aes(x = x, y = y, fill = .pred_TRUE)) + 
      geom_raster() + 
      coord_quickmap() +
      scale_fill_viridis(option = "turbo", limits = c(0, 1)) +
      labs(title = d, fill = "Probability", y = "Latitude", x = "Longitude") +
      theme_bw() + 
      theme(legend.position = "bottom")
    
    # Save the created plot as a png to the tmp directory
    ggsave(sprintf(png_path, i + starting_i), plot = p, width = 6, height = 5, dpi = 150)
  }
  TRUE
}

# Predicts and saves gif of workflow predicted onto coper_data_predictable to file
save_workflow_gif <- function(workflow, filename) {
  preds <- augment(workflow, coper_data_predictable, type = "prob") |>
    dplyr::select(lon, lat, date, .pred_TRUE)
  
  data_cube_gif(preds |> rename(x = lon, y = lat), starting_i = 0)
  
  rm(preds)
  gc()
  
  # Retrieve a list of every frame file in the tmp directory
  frame_files <- Sys.glob(file.path(frame_dir, "frame*.png"))
  # Collapse the list into a single string separated by spaces
  frame_list <- paste(shQuote(frame_files), collapse = " ")
  
  # Using convert provided by ImageMagick CLI tools to combine pngs into gif
  gif_output <- paste0("model_exploration/representing_seasonality/gifs/", filename, ".gif")
  convert_command <- sprintf("convert -delay 25 -loop 0 %s %s", frame_list, shQuote(gif_output))
  system(convert_command)
  
  # Remove the tmp files
  file.remove(frame_files)
}

save_workflow_gif(workflow_list[[3]], "DAY_LENGTH_DDX_SPRING2")

walk2(workflow_list, workflow_savenames, save_workflow_gif)

