library(copernicus)
library(andreas)
library(raster)
library(dplyr)
library(stars)

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

## Retrieve copernicus data for example date
example_date <- "2015-07-01"
coper_path <- copernicus_path("nwa/GLOBAL_MULTIYEAR_PHY_001_030")
coper_DB <- coper_path |> read_database() |>
  filter(date == as.Date(example_date))
coper_data <- read_andreas(coper_DB, coper_path)

## Retrieve bathymetry
## https://www.ncei.noaa.gov/products/etopo-global-relief-model
## Dimensions retrieved based on coper_data
## st_get_dimension_values(coper_data, 'x', where = "start") |> range()
bathy_raster <- raster(file.path(root, "etopo_bathymetry.tiff"))
crs(bathy_raster)
extent(bathy_raster)

bathy_stars <- st_as_stars(bathy_raster, att = 1, ignore_file = FALSE)
bs_downsampled <- st_downsample(bathy_stars, n = 9, FUN = mean)

# range of bs_downsampled must be Larger than that of coper_data
st_dimensions(bs_downsampled)
st_dimensions(coper_data)

