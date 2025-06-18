# https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_PHY_001_024/description

# depth, surface mld bottom
# sst anomaly
# mlotst -- mixed layer depth, meters

library(copernicus)
library(andreas)
library(stars)
library(dplyr)

# /mnt/ecocast/coredata/copernicus/nwa/GLOBAL_ANALYSISFORECAST_PHY_001_024

# Sets root path, defaults for ecocast. Should only have to run once
copernicus::set_root_path()
path <- copernicus::copernicus_path("nwa/GLOBAL_ANALYSISFORECAST_PHY_001_024")

# Lookup tables - optional if shopping for data
lut = read_product_lut(product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024') |>
  glimpse()

# Read in database from data path
DB <- read_database(path)

db_test <- DB |> filter(date == '2022-06-01')

db <- slice(DB, 1:12)
res <- read_andreas(db, path)




