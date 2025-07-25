source("setup.R")
library(ecomon)
library(twinkle)

# Input data preparation. 
# Requisite columns: lat, lon, date, ind_m2
jelly_raw <- ecomon::scale_ecomon() |> 
  select(c(lon, lat, date, ind_m2 = coel_m2)) |>
  na.omit()

cfin_raw <- "/mnt/ecocast/projectdata/calanusclimate/src/vertical_correction_ecomon.csv.gz" |>
  readr::read_csv(col_types = readr::cols()) |>
  select(lon = longitude, lat = latitude, date, ind_m2 = corrected_CIV_CVI_m2)


#' Matches abundance records in a dataset to copernicus values
#' @param abund_data df, abundance values. Required columns lon, lat, date, ind_m2
#' @param type str, either 'phys' or 'bgc
#' @param add_bathy bool, also retrieve and tack on bathymetry data? 
#' @return df, matched abundance records with original columns + physical covariates
match_coper_abundance <- function(abund_data, 
                                  type = c("phys", "bgc")[[1]], 
                                  add_bathy = TRUE) {
  ###### Setup
  coper_info <- NULL
  if (type == "phys") {
    coper_info <- get_coper_info(region = "chfc", type = "phys")
  } else if (type == "bgc") {
    coper_info <- get_coper_info(region = "world", type = "bgc")
  } else {
    stop("Type ", type, " not supported")
  }
  
  # Cropping data to match spatial and temporal dims of copernicus
  abund_cropped <- abund_data |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    filter(date >= coper_info$date_range[[1]], # Removes about 10K
           date <= coper_info$date_range[[2]])
  if (type == "phys") {
    abund_cropped <- abund_cropped |>
      st_crop(coper_info$bbox)
  }
  
  # How many dates are represented by these points?
  full_date_vec <- abund_cropped$date |> unique()
  
  ##### Retrieve ocean-based Copernicus coordinate locations for abund records
  template_date <- full_date_vec[[1]]
  coper_template <- read_andreas(coper_info$meta_db |> filter(date == template_date), 
                                 coper_info$coper_path)
  
  # Using twinkle package to extract nearest ocean-based coordinate
  lut_matched <- st_extract(twinkle::make_raster_lut(coper_template), 
                            at = abund_cropped)
  coper_xy <- st_coordinates(coper_template)[lut_matched$index,]
  # Geometry now points to a guaranteed ocean point in copernicus
  abund_matchable <- bind_cols(
    abund_cropped |> st_drop_geometry(),
    st_coordinates(abund_cropped) |> data.frame() |> rename(olon = X, olat = Y),
    coper_xy |> st_as_sf(coords = c("x", "y"), crs = 4326)) |>
    st_as_sf()
  
  ##### Matching abundance data to Copernicus over date
  
  # Creating smaller chunks of dates for processing
  # size_max calculated based on max size in GB and size of single layer
  size_max <- (7 * 1e9/object.size(coper_template)) |> as.numeric() |> floor()
  date_split <- split(full_date_vec, ceiling(seq_along(full_date_vec)/size_max))
  
  #' Matches all abundance values to copernicus values for a specific date range
  #' @param date_vec vct, list of dates. Max size determined by memory, suggested 1000
  #' @return df, list of all abundance records matched to copernicus information
  match_date_chunk <- function(date_vec) {
    print(paste0("Processing date chunk, size ", length(date_vec), "..."))
    
    ## Filter so dates are identical
    coper_db <- coper_info$meta_db |> filter(date %in% date_vec)
    coper_data <- read_andreas(coper_db, coper_info$coper_path) # takes a second
    
    ## Filter abundance data 
    abund_datecropped <- abund_matchable |> filter(date %in% date_vec)
    
    # Extract
    coper_matched <- st_extract(coper_data,
                                at = abund_datecropped, 
                                time_column = "date")
    
    res <- bind_cols(abund_datecropped, 
                     st_drop_geometry(coper_matched) |> select(-date))
    
    rm(coper_data, coper_db, abund_datecropped, coper_matched)
    gc()
    
    return(res)
  }
  
  abund_coper_matched <- date_split |>
    lapply(match_date_chunk) |>
    bind_rows()
  
  ####### Adding static bathymetry
  if (add_bathy) {
    bdepth_stars <- read_static(name = "deptho", path = coper_info$coper_path)
    abund_coper_matched <- abund_coper_matched |>
      bind_cols(st_extract(bdepth_stars, at = abund_matchable) |>
                  rename(bathy_depth = deptho) |>
                  st_drop_geometry())
  }
  
  # Reformatting and adding day_of_year variable
  abund_coper_matched |>
    st_drop_geometry() |>
    rename(lat = olat, lon = olon) |>
    mutate(day_of_year = lubridate::yday(date), .after = date)
}

jelly_phys <- jelly_raw |>
  match_coper_abundance(type = "phys", add_bathy = TRUE)
jelly_results_all <- jelly_phys |>
  match_coper_abundance(type = "bgc", add_bathy = FALSE) |>
  select(-time)

cfin_results <- cfin_raw |>
  match_abundance_physical() |>
  rename(corrected_cfin_m2 = ind_m2)
  
## Saving to file
root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus"

readr::write_csv(jelly_results,
                 file = file.path(root, 
                                  "input_data/jelly_coper_phys_revised.csv.gz"))
readr::write_csv(jec2,
                 file = file.path(root, 
                                  "input_data/jelly_coper_all_revised.csv.gz"))

readr::write_csv(cfin_results, 
                 file = file.path(root, "input_data/matched_ecomon_copernicus_physical_chfc.csv.gz"))
