source("setup.R")

# Matches abundance data to ETOPO bathymetry values saved to file
jelly_abund <- get_input_data("matched_ecomon_copernicus_physical_chfc.csv.gz")
cfin_abund <- get_input_data("matched_jellyfish_copernicus_phys_bgc_chfc.csv.gz")

#' Matches abundance data to etopo bathymetry values
#' @param abund_data df, abundance data. Required columns lon, lat
#' @param keep_search_radius bool, keep search radius for analysis? 
#' @return df, abund_data with added column bathy_depth
match_abundance_bathy_ET <- function(abund_data, keep_search_radius = FALSE) {
  etopo_bathy <- get_input_data("bathymetry/bathy_depth_ET_chfc.csv.gz")
  
  abund_joinable <- abund_data |>
    rename(x = lon, y = lat) |>
    mutate(across(c("x", "y"), ~round(.x * 12), .names = "{.col}.12"))
  etopo_bathy_joinable <- etopo_bathy |>
    mutate(across(c("x", "y"), ~round(.x * 12), .names = "{.col}.12"))
  
  abund_bathy_et <- left_join(abund_joinable, etopo_bathy_joinable,
                              by = c("x.12", "y.12"))
  
  # Handle instances where bathymetry was incorrectly assigned
  abund_bathy_et <- 
    bfs_mismatched_indices(which(is.na(abund_bathy_et$bathy_depth)), 
                           abund_bathy_et, 
                           etopo_bathy_joinable,
                           .col = "bathy_depth", 
                           .match_cols = list("lon" = "x.12", "lat" = "y.12")) |>
    dplyr::select(-(x.12:y.y)) |>
    rename(lat = y.x, lon = x.x)
  
  # Returning
  if (!keep_search_radius) {
    abund_bathy_et <- abund_bathy_et |>
      select(-search_radius)
  }
  
  abund_bathy_et
}

#' Matches abundance data to brickman bathymetry values
#' @param abund_data df, abundance data. Required columns lon, lat
#' @param generate_brickman bool, do we need to generate new brickman values or read from file? 
match_abundance_bathy_BR <- function(abund_data, generate_brickman = TRUE) {
  br_bathy <- NULL
  if (generate_brickman) {
    abund_sf <- st_as_sf(abund_data, coords = c("lon", "lat"), crs = 4326)
    
    br_bathy <- brickman::extract_points(brickman::compose_filename("PRESENT"), 
                                         "Bathy_depth", 
                                         mec_sf, 
                                         complete = TRUE,
                                         simplify_names = FALSE)
    
    # br_bathy <- mutate(br_bathy, estimate_offset = sqrt((lon-olon)^2 + (lat-olat)^2))
    
    readr::write_csv(br_bathy, file = file.path(root, "brickman_bathymetry.csv.gz"))
  } else {
    br_bathy <- get_input_data("brickman_bathymetry.csv.gz")
  }
  
  abund_data |>
    mutate(bathy_depth = br_bathy$Bathy_depth)
}
  
##### Saving to file

jelly_results <- jelly_abund |>
  match_abundance_bathy_ET()

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data"
readr::write_csv(jelly_results, 
                 file = file.path(root, "jelly_phys_bgc_bathy_ET_chfc.csv.gz"))
