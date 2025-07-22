library(raster)
library(stars)

source("old_setup.R")

# Creates a datafile for July 1st, 2015 env covariates and matches etopo bathymetry
# Plots etopo bathymetry displacement value based on land-based matching
# Compares etopo and brickman datasets

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data"

## Copernicus Data for example date: July 1st, 2015
example_date <- "2015-07-01"

coper_info <- get_coper_info("chfc", "phys")

coper_db <- coper_info$meta_db |>
  filter(date == as.Date(example_date))
coper_data <- read_andreas(coper_db, coper_info$coper_path) 

## Bathymetry TIFF
# https://www.ncei.noaa.gov/products/etopo-global-relief-model
# Dimensions to retrieve specified based on coper_data range
# st_bbox(coper_data)
bathymetry_tiff <- raster(file.path(root, "bathymetry/etopo_bathymetry_chfc.tiff"))

bathy_stars <- st_as_stars(bathymetry_tiff, att = 1, ignore_file = FALSE) |>
  st_warp(coper_data, method = "bilinear", use_gdal = TRUE, no_data_value = 99999) 
names(bathy_stars) <- "etopo_bathy"
bathy_depth_stars <- bathy_stars |>
  transmute(bathy_depth = ifelse(etopo_bathy <= 0, etopo_bathy * -1, NA))

coper_bathy <- coper_data
coper_bathy$bathy_depth <- bathy_depth_stars
plot(coper_bathy[8])

### NA examination
cb_tib <- coper_bathy |>
  as_tibble() |>
  mutate(across(c(x, y), ~round(.x * 12), .names = "{.col}.12"))
bathy_depth_tib <- bathy_depth_stars |>
  as_tibble() |>
  mutate(across(c(x, y), ~round(.x * 12), .names = "{.col}.12"))
  
# Which rows have incorrectly marked NA bathymetry? n = 707
coper_complete <- which(complete.cases(dplyr::select(cb_tib, -bathy_depth)))
bathy_nas <- which(is.na(cb_tib$bathy_depth))
na_matched_indices <- coper_complete[coper_complete %in% bathy_nas]

fixed_cb_tib <- bfs_mismatched_indices(na_matched_indices, 
                                       cb_tib, 
                                       bathy_depth_tib,
                                       .col = "bathy_depth", 
                                       .match_cols = list("lon" = "x.12", 
                                                          "lat" = "y.12"))
plot_base <- fixed_cb_tib |>
  filter(!is.na(search_radius)) |>
  ggplot() + 
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", col = "gray") +
  coord_quickmap(xlim = c(-76, -50), ylim = c(35, 57), expand = TRUE) +
  theme_bw() + 
  theme(legend.position = "bottom")

displacement_chart <- plot_base +
  geom_segment(aes(x = x, y = y, xend = x.12/12, yend = y.12/12, color = search_radius),
               arrow = arrow(length = unit(0.1,"cm")), linewidth = .2) +
  labs(x = "Longitude", y = "Latitude", col = "Search Radius (x * 1/12°)") + 
  scale_color_viridis(direction = -1) + 
  ggtitle("Bathymetry Displacement")
na_matched_bathy <- plot_base + 
  geom_point(aes(x = x, y = y, col = bathy_depth)) + 
  scale_color_viridis(option = "turbo") + 
  labs(x = "Longitude", y = "Latitude", col = "Bathymetry (fixed)") + 
  ggtitle("NA-fixed Bathymetry Locations")

save_pdf_ecocast(list(na_matched_bathy, displacement_chart), 
                 "plots/fullrange_NAmatchedbathyfix_ET_coper_chfc", root)

### Saving materials to file

predictable_fixed_cb <- fixed_cb_tib |> dplyr::select(x:bathy_depth)
predictable_fixed_cb <- predictable_fixed_cb[complete.cases(predictable_fixed_cb),]
predictable_fixed_cb |>
  readr::write_csv(file.path(root, "2015_07_01_exdata_ET.csv.gz"))

bathy_depth_stars |>
  as_tibble() |>
  readr::write_csv(file.path(root, "bathymetry/bathy_depth_ET_chfc.csv.gz"))
bathy_depth_stars |>
  write_stars(file.path(root, "bathymetry/bathy_depth_ET_chfc.tif"))

## Comparing to brickman data
if (FALSE) {
  library(fuzzyjoin)
  library(ggplot2)
  library(viridis)
  
  verification_tib <- as_tibble(coper_bathy) |>
    rename(lon = x, lat = y) # bathy_depth
  
  # Just a normal plot
  ggplot(verification_tib, aes(x = lon, y = lat, fill = bathy_depth)) + 
    geom_raster() + 
    coord_quickmap() +
    scale_fill_viridis(option = "turbo")
  
  comparison_tib <- readr::read_csv(file.path(root, "bathymetry/brickman_bathymetry.csv.gz"))
  comparison_tib <- comparison_tib |>
    mutate(across(c(lon, lat), ~round(.x * 12)/12))
  # Bathy_depth
  
  # merged <- left_join(comparison_tib, verification_tib, by = c('lon'='x', 'lat'='y'))
  merged <- difference_left_join(comparison_tib, verification_tib, 
                                 by = c('lon', 'lat'), max_dist = .001)
  merged <- merged |>
    mutate(bathy_diff = Bathy_depth - bathy_depth)
  
  plot_gen(merged, 'bathy_depth', xy_names = c("lon.x", "lat.x"))
  plot_gen(merged, 'bathy_diff', xy_names = c("lon.x", "lat.x"))
  
  merged |> arrange(desc(bathy_diff))
  
  sigdiff_merged <- merged |>
    filter(abs(bathy_diff) > 15)
  
  p <- plot_gen(sigdiff_merged, 'bathy_diff', xy_names = c("lon.x", "lat.x"),
           title = paste0("Brickman - Etopo, n = ", nrow(sigdiff_merged), "/", nrow(merged)))
  
  save_pdf_ecocast(p, "bathymetry/brickman_etopo_comparison", root)
}

