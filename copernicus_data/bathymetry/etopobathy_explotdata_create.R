library(raster)
library(copernicus)
library(andreas)
library(dplyr)
library(stars)

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

## Copernicus Data for example date: July 1st, 2015
example_date <- "2015-07-01"

coper_path <- copernicus_path("nwa/GLOBAL_MULTIYEAR_PHY_001_030")
coper_db <- coper_path |> read_database() |>
  filter(date == as.Date(example_date))
coper_data <- read_andreas(coper_db, coper_path) 

## Bathymetry TIFF
# https://www.ncei.noaa.gov/products/etopo-global-relief-model
# Dimensions to retrieve specified based on coper_data range
# st_bbox(coper_data)
bathymetry_tiff <- raster(file.path(root, "bathymetry/etopo_bathymetry.tiff"))

bathy_stars <- st_as_stars(bathymetry_tiff, att = 1, ignore_file = FALSE) |>
  st_warp(coper_data, method = "bilinear", use_gdal = TRUE, no_data_value = 99999) 
names(bathy_stars) <- "etopo_bathy"
bathy_depth_stars <- bathy_stars |>
  transmute(bathy_depth = ifelse(etopo_bathy <= 0, etopo_bathy * -1, NA))

coper_bathy <- coper_data
coper_bathy$bathy_depth <- bathy_depth_stars
plot(coper_bathy[8])

coper_bathy |>
  as_tibble() |>
  readr::write_csv(file.path(root, "2015_07_01_exdata_etopo.csv.gz"))

bathy_depth_stars |>
  as_tibble() |>
  readr::write_csv(file.path(root, "bathymetry/bathy_depth_ET_nwa.csv.gz"))
bathy_depth_stars |>
  write_stars(file.path(root, "bathymetry/bathy_depth_ET_nwa.tif"))

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

