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
season_vector <- c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1)
ecomon <- data_from_config(config) |>
  filter(date < as.Date("2014-12-31"))
ecomon_sf <- st_as_sf(ecomon, crs = 4326, coords = c("lon", "lat")) |>
  st_join(regions) |>
  na.omit() |>
  mutate(month = lubridate::month(date), 
         year = lubridate::year(date),
         patch = as.numeric(patch) - 1)
ecomon_sf <- ecomon_sf |>
  mutate(season = season_vector[month],
         yearmonth = year + season/5)

over_time_yearmonth <- ecomon_sf |>
  group_by(yearmonth, id) |>
  summarize(meanpatch = mean(patch), 
            numentries = n())

over_time_month <- ecomon_sf |>
  group_by(month, id) |>
  summarize(meanpatch = mean(patch), 
            numentries = n())

ggplot(over_time_month, 
       aes(x = month, y = meanpatch)) + 
  facet_wrap(~id) +
  geom_line() + 
  geom_point(aes(size = numentries)) + 
  theme_bw()

## Plot coper_stars data over region and time
pred_root <- v_path(v, "preds", "2day_resolution_15fold")
predfiles <- list.files(pred_root, full.names = TRUE)
predfiles <- predfiles[-length(predfiles)]
names(predfiles) <- 1993:2014

process_year <- function(year) {
  # Load and prepare the stars object
  year_preds <- read_quantile_stars(predfiles[year])
  
  # Aggregate spatially by region
  year_preds <- aggregate(year_preds, by = regions, FUN = mean, na.rm = TRUE)
  
  # Convert stars object to data.frame, keep only region ID and band values
  summary_df <- as.data.frame(year_preds, long = TRUE)
  summary_df <- summary_df |> 
    mutate(month = lubridate::month(date))
  
  over_time_month_stars <- summary_df |>
    group_by(month, geometry) |>
    summarize(meanpatch = mean(`50%`), 
              numentries = n()) |>
    left_join(regions) |>
    select(-geometry)
  
  rm(year_preds, summary_df)
  gc()
  over_time_month_stars
}

all_years <- names(predfiles) |>
  map(process_year) |>
  bind_rows()

coperpredres <- all_years |>
  group_by(month, id) |>
  summarize(meanpred = weighted.mean(meanpatch, numentries),
            totalpredcount = sum(numentries), .groups = "keep")

ggplot() +
  geom_line(data = over_time_month, aes(x = month, y = meanpatch)) + 
  geom_point(data = over_time_month, aes(x = month, y = meanpatch, size = numentries)) +
  geom_line(data = coperpredres |> filter(id %in% c("MAB", "GoM", "WSS")), 
            aes(x = month, y = 1-meanpred), col = "red") +
  facet_wrap(~id) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Month", y = "Mean patch value / patch prediction", size = "No. Records")









