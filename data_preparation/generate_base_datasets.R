species <- "" # Start undefined
source("setup.R")
library(ecomon)
library(twinkle)

## Define abundance datasets for species not pulling from scale::ecomon()
cfin_df <- "/mnt/ecocast/projectdata/calanusclimate/src/vertical_correction_ecomon.csv.gz" |>
  readr::read_csv(col_types = readr::cols()) |>
  select(lon = longitude, lat = latitude, date, ind_m2 = corrected_CIV_CVI_m2)


#' Helper: Matches abundance records in a dataset to all available copernicus values. 
#' Reference for static variables is physical copernicus.
#' If new variables become available, this function must be modified. 
#' @param spec_name str, species name
#' @param abundance_df df, abundance values. Required columns lon, lat, date
#' @param verbose bool, print progress?
#' @return df, matched abundance records with original columns + physical covariates
match_dataset_copernicus <- function(abundance_df, verbose = TRUE) {
  if (!all(c("lon", "lat", "date") %in% colnames(abundance_df))) {
    stop("Abundance data frame requires columns lon, lat, and date.")
  }
  
  if (verbose) {cat("Starting matching...")}
  coper_infos <- list(
    "phys" = get_coper_info(region = "chfc", type = "phys"), 
    "bgc" = get_coper_info(region = "world", type = "bgc")
  )
  bathy_slope_file <- get_path_main("_general_data", "bathymetric_slope_terra.tif")
  
  # Spatially and temporally crop the data
  abund_sf <- abundance_df |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    filter(between(date, coper_infos$phys$date_range[[1]], coper_infos$phys$date_range[[2]]),
           between(date, coper_infos$bgc$date_range[[1]], coper_infos$bgc$date_range[[2]])) |>
    st_crop(coper_infos$phys$bbox) |>
    suppressWarnings()
  
  # What dates are in this dataset?
  full_date_vec <- abund_sf$date |> unique()
  template_date <- full_date_vec[[1]]
  
  #' Helper: Matches copernicus records from a specific coper_info object
  #' Adds static variables for type = phys, region = chfc
  #' @param abund_sf sf, abundance sf object
  #' @param coper_info copernicus info object
  #' @return abundance sf with added columns from matched copernicus 
  match_abund_copertype <- function(abund_sf, coper_info) {
    if (verbose) {cat("\r Matching type:", coper_info$type)}
    
    # Alter geometries to point towards nearest ocean-based coper coordinate
    # Leverages make_raster_lut from ben's twinkle package
    coper_template <- read_andreas(
      coper_info$meta_db |> filter(date == template_date), coper_info$coper_path)
    lut_matched <- st_extract(twinkle::make_raster_lut(coper_template), 
                              at = abund_sf)
    matchable_geometries <- st_coordinates(coper_template)[lut_matched$index,] |>
      st_as_sf(coords = c("x", "y"), crs = 4326)
    # Geometry now points to a guaranteed ocean point in copernicus
    abund_matchable <- bind_cols(
      abund_sf |> st_drop_geometry(),
      st_coordinates(abund_sf) |> data.frame() |> rename(olon = X, olat = Y),
      matchable_geometries) |>
      st_as_sf()
    
    ##### Matching abundance data to dynamic Copernicus over date
    
    # Creating smaller chunks of dates for processing
    # size_max calculated based on max size in GB and size of single layer
    size_max <- (7 * 1e9/object.size(coper_template)) |> as.numeric() |> floor()
    date_split <- split(full_date_vec, ceiling(seq_along(full_date_vec)/size_max))
    
    #' Matches all abundance values to copernicus values for a specific date range
    #' @param date_vec vct, list of dates.
    #' @return df, list of all abundance records matched to copernicus information
    match_date_chunk <- function(date_vec, index) {
      if (verbose) {cat("\r Processing", index, "/", length(date_split), coper_info$type, "chunks...")}
      
      ## Filter so dates are identical
      coper_data <- read_andreas(coper_info$meta_db |> filter(date %in% date_vec), 
                                 coper_info$coper_path) # takes a second
      
      ## Filter abundance data 
      abund_datecropped <- abund_matchable |> filter(date %in% date_vec)
      
      # Extract
      coper_matched <- st_extract(coper_data,
                                  at = abund_datecropped, 
                                  time_column = "date")
      
      res <- bind_cols(abund_datecropped, 
                       st_drop_geometry(coper_matched) |> select(-date))
      
      rm(coper_data, abund_datecropped, coper_matched)
      gc()
      
      return(res)
    }
    
    # Process in date chunks to add copernicus variables
    abund_copertype_matched <- date_split |>
      imap(match_date_chunk) |>
      bind_rows()
    
    if (verbose) {cat("\r Formatting to return", coper_info$type)}
    
    ####### Matching abundance data to static Copernicus, if using static ref
    if (coper_info$type == "phys" && coper_info$region == "chfc") {
      static_coper <- read_static(path = coper_info$coper_path)
      static_coper$bathy_slope <- read_stars(bathy_slope_file)
      
      extracted_static <- st_extract(static_coper, at = abund_matchable) |>
        rename(bathy_depth = deptho) |>
        st_drop_geometry()
      
      abund_copertype_matched <- abund_copertype_matched |>
        bind_cols(extracted_static)
    }
    
    abund_copertype_matched |>
      select(-time) |>
      st_drop_geometry() |>
      st_as_sf(coords = c("olon", "olat"), crs = 4326) 
  }
  
  matched_abund_phys <- abund_sf |>
    match_abund_copertype(coper_infos$phys) 
  gc()
  matched_abund_allcoper <- matched_abund_phys |>
    match_abund_copertype(coper_infos$bgc)
  gc()
  
  returnable_mac <- 
    bind_cols(st_coordinates(matched_abund_allcoper),
              st_drop_geometry(matched_abund_allcoper)) |>
    mutate(day_of_year = lubridate::yday(date)) |>
    rename(lon = X, lat = Y)
  
  returnable_mac
}

#' Matches a specific species and saves to file. 
#' @param species str, species name
#' @param abundance_df df, abundance values. Required columns lon, lat, date, ind_m2
#' @param verbose bool, print progress?
#' @return filepath to saved object
match_species_copernicus <- function(species, abundance_df, verbose = TRUE) {
  if (!all(c("lon", "lat", "date", "ind_m2") %in% colnames(abundance_df))) {
    stop("Abundance data frame requires columns lon, lat, date, and ind_m2.")
  }
  
  mac <- match_dataset_copernicus(abundance_df, verbose = verbose)
  filepath <- file.path(get_root(species), "input_data", paste0(species, "_copernicus_matched.csv.gz"))
  readr::write_csv(mac, filepath)
  
  filepath
}

#' Matches ecomon dataframe with copernicus and saves to general data file
#' @param ecomon_df df, ecomon dataset
#' @param verbose bool, print progress?
#' @return filepath to saved object
match_ecomon_copernicus <- function(ecomon_df = ecomon::scale_ecomon(), verbose = TRUE) {
  if ("time" %in% colnames(ecomon_df)) {
    ecomon_df <- select(ecomon_df, -time)
  }
  mec <- match_dataset_copernicus(ecomon_df, verbose = verbose)
  filepath <- get_path_main("_general_data", "ecomon_copernicus_matched.csv.gz")
  readr::write_csv(mec, filepath)
  
  filepath
}



