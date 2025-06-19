library(brickman)
library(dplyr)
library(sf)

# Copernicus does not serve bathymetry data. For now, pulling from Brickman

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"

mec <- get_input_data("matched_ecomon_copernicus_physical_nwa.csv.gz")

######### Retrieve bathymetry data for a new dataset with new lat/lon coords

mec_sf <- st_as_sf(mec, coords = c("longitude", "latitude"), crs = 4326)

# Merge with Brickman "Bathy_depth"
brickman_source <- brickman::compose_filename("PRESENT")

bdat <- brickman::extract_points(brickman_source, 
                                 "Bathy_depth", 
                                 mec_sf, 
                                 complete = TRUE,
                                 simplify_names = FALSE)

# How far away, in degrees, is the bathymetry estimate from actual coords?
bbat <- bdat |>
  mutate(estimate_offset = sqrt((lon-olon)^2 + (lat-olat)^2))

readr::write_csv(bbat, file = file.path(root, "brickman_bathymetry.csv.gz"))

######## Pull existing brickman bathymetry data and add it back

bbat <- readr::read_csv(file.path(root, "brickman_bathymetry.csv.gz"))

mec_bathy_matched <- mec |>
  mutate(bathymetry = bbat$Bathy_depth)

readr::write_csv(mec_bathy_matched, file = file.path(root, "mec_brickman_bathy.csv.gz"))



