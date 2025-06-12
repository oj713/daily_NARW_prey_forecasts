library(raster)
library(copernicus)
library(andreas)

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
# coper_data |> st_get_dimension_values('y', where = "start") |> range()
# coper_data |> st_get_dimension_values('x', where = "start") |> range()
bathymetry_tiff <- raster(file.path(root, "etopo_bathymetry.tiff"))

# Obtain details
crs(bathymetry_tiff)
extent(bathymetry_tiff)

bathy_stars <- st_as_stars(bathymetry_tiff, att = 1, ignore_file = FALSE)

## Enabling merge of data layers

# bathymetry must be significantly downsampled to match resolution
bs_downsampled <- st_downsample(bathy_stars, n = 9, FUN = mean)
st_dimensions(bs_downsampled)

# test combine 
coper_bathy <- coper_data
coper_bathy$bathymetry <- bs_downsampled
