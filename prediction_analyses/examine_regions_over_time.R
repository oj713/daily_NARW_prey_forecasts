species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")
source("generate_data_cubes.R")

v <- "je.0.00"
config <- read_config(v)

## Retrieve region polygons
regions <- read_sf(dsn = "prediction_analyses/region_shapefiles/johnson2024_regions.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)
ordered_reg <- c("MAB", "GoM", "WSS", "ESS", "swGSL", "nGSL", "NLS")

ggplot() +
  geom_sf(data = regions, aes(fill = factor(id, levels = ordered_reg)), 
          col = "black", alpha=.3, show.legend = FALSE) +
  scale_fill_manual(values = c("#00B6EB", "#FB61D7", "#00C094", "#A58AFF",
                               "#53B400", "#F8766D", "#C49A00")) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group),
               fill = "lightgray", col = "gray") +
  coord_sf(xlim = c(-75, -40), ylim = c(35, 55), expand = TRUE) +
  theme_bw() + 
  ggtitle("Regions") + 
  labs(x = "lon", y = "lat", fill = "Region")

## Plot ecomon dataset over region and time
ecomon <- data_from_config(config)
ecomon_sf <- st_as_sf(ecomon, crs = 4326, coords = c("lon", "lat")) |>
  st_join(regions) |>
  na.omit() |>
  mutate(month = lubridate::month(date), 
         year = lubridate::year(date),
         patch = as.numeric(patch) - 1)

over_time <- ecomon_sf |>
  group_by(year, month, id) |>
  summarize(meanpatch = mean(patch))

over_time_yearly <- ecomon_sf |> 
  group_by(year, id) |>
  summarize(meanpatch = mean(patch))

ggplot(over_time_yearly, 
       aes(x = year, y = meanpatch)) + 
  facet_wrap(~id) +
  geom_line()

## Plot coper_stars data over region and time
pred_root <- v_path(v, "preds", "3day_resolution_2fold")
years <- list.files(pred_root) |> as.numeric()

regions <- st_read("prediction_analyses/region_shapefiles/johnson2024_regions.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)

process_year <- function(year) {
  # Load and prepare the stars object
  year_preds <- read_quantile_stars(file.path(pred_root, year))[[1]] |>
    st_set_crs(4326)
  
  # Aggregate spatially by region
  summary_stars <- aggregate(year_preds, by = regions, FUN = mean, na.rm = TRUE)
  
  # Convert stars object to data.frame, keep only region ID and band values
  summary_df <- as.data.frame(summary_stars, long = TRUE) |>
    setNames(c("geometry", "band", "average"))
    dplyr::select(region_id = , band, value = 3)  # replace 'region_column_name' and '3' as appropriate
  
  # Collapse across bands (mean per region)
  summary_wide <- summary_df |>
    group_by(region_id) |>
    summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = region_id, values_from = mean_value)
  
  # Add year column
  summary_wide <- summary_wide |>
    mutate(year = year) |>
    relocate(year)
  
  return(summary_wide)
}





