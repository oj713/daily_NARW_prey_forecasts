library(dplyr)
library(sf)

# Matching data to etopo bathymetry coordinates

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

mec <- readr::read_csv(file.path(root, "matched_ecomon_copernicus_physical_nwa.csv.gz"))
etopo_bathy <- readr::read_csv(file.path(root, "bathymetry/etopo_bathy_depth_nwa.csv.gz"))

# Converting each coordinate to integer values representing degree position in twelfths
mec_joinable <- mec |>
  mutate(across(c(longitude, latitude), ~round(.x * 12), .names = "{.col}.12"))
etopo_bathy_joinable <- etopo_bathy |>
  mutate(across(c("x", "y"), ~round(.x * 12), .names = "{.col}.12"))

mec_etbathy <- left_join(mec_joinable, etopo_bathy_joinable,
                         by = c("longitude.12" = "x.12", "latitude.12" = "y.12"))

mec_etbathy <- mec_etbathy |>
  select(-(longitude.12:y)) # removing unnecessary variables

readr::write_csv(mec_etbathy, file = file.path(root, "mec_bathy_ET.csv.gz"))
