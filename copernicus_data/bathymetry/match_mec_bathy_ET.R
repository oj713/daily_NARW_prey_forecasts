source("setup.R")

# Matching data to etopo bathymetry coordinates

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

mec <- readr::read_csv(file.path(root, "matched_ecomon_copernicus_physical_nwa.csv.gz"))
etopo_bathy <- readr::read_csv(file.path(root, "bathymetry/bathy_depth_ET_nwa.csv.gz"))

# Converting each coordinate to integer values representing degree position in twelfths
mec_joinable <- mec |>
  rename(x = longitude, y = latitude) |>
  mutate(across(c("x", "y"), ~round(.x * 12), .names = "{.col}.12"))
etopo_bathy_joinable <- etopo_bathy |>
  mutate(across(c("x", "y"), ~round(.x * 12), .names = "{.col}.12"))

mec_etbathy <- left_join(mec_joinable, etopo_bathy_joinable,
                         by = c("x.12", "y.12"))

# Handle instances where bathymetry was incorrectly assigned
fixed_mec_etbathy <- 
  bfs_mismatched_indices(which(is.na(mec_etbathy$bathy_depth)), 
                         mec_etbathy, 
                         etopo_bathy_joinable,
                         .col = "bathy_depth", 
                         .match_cols = list("lon" = "x.12", "lat" = "y.12"))
  
##### Saving to file

writable_mec_etbathy <- fixed_mec_etbathy |>
  dplyr::select(-(x.12:y.y), -search_radius) |>
  rename(latitude = y.x, longitude = x.x) # removing unnecessary variables

readr::write_csv(writable_mec_etbathy, 
                 file = file.path(root, "mec_bathy_ET.csv.gz"))
