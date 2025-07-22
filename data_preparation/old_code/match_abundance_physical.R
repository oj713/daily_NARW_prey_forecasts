source("old_setup.R")
library(ecomon)

# Input data preparation. 
# Requisite columns: lat, lon, date, ind_m2
jelly_raw <- ecomon::scale_ecomon() |> 
  select(c(lon, lat, date, depth, ind_m2 = coel_m2)) |>
  na.omit()

cfin_raw <- "/mnt/ecocast/projectdata/calanusclimate/src/vertical_correction_ecomon.csv.gz" |>
  readr::read_csv(col_types = readr::cols()) |>
  select(lon = longitude, lat = latitude, date, ind_m2 = corrected_CIV_CVI_m2)

#' Matches abundance records in a dataset to copernicus physical values
#' @param abund_data df, abundance values. Required columns lon, lat, date, ind_m2
#' @param region str, either "chfc" or "nwa" depending on target region
#' @return df, matched abundance records with original columns + physical covariates
match_abundance_physical <- function(abund_data, region = "chfc") {
  # Retrieve copernicus information
  coper_info <- get_coper_info("chfc")
  cbbox <- coper_info$bbox
  
  # Cropping data to match spatial and temporal dims of copernicus
  abund_cropped <- abund_data |> # st_crop(abunddata, coper_info$bbox) |> # Removes 7
    filter(lat >= cbbox$ymin, lat <= cbbox$ymax,
           lon >= cbbox$xmin, lon <= cbbox$xmax,
           date >= coper_info$date_range[[1]], # Removes about 10K
           date <= coper_info$date_range[[2]]) |>
    mutate(x_index = coper_info$get_xy_index(lon, axis = "x"),
           y_index = coper_info$get_xy_index(lat, axis = "y"))
  
  # How many dates are represented by these points?
  full_date_vec <- abund_cropped$date |> unique()
  # Creating smaller chunks of dates, max size 1000, for processing
  size_max <- 1000
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
    abund_datecropped <- abund_cropped |> filter(date %in% date_vec)
    
    abund_groups <- abund_datecropped |> group_by(date) |> group_split()
    
    #' Helper that retrieves the copernicus records for all abund records on a specific day
    #' @param day int, the index of the date to retrieve
    #' @returns df, lat/lon/date/abundance and matched copernicus lat/lon/env values
    match_date <- function(day) {
      date_group <- abund_groups[[day]]
      
      #' Retrieves ocean copernicus data based on lon and lat indices
      #' Also handles instances where land-based match is made by accident
      retrieve_coper_tibble <- function(x_index, y_index, ...) {
        coper_tibble <- coper_data[, x_index, y_index, day] |> as_tibble()
        
        # In rare instances, the closest copernicus point might be an NA land-based value
        # This is basically the same thing as bfs_mismatched_indices() in setup.R
        if (coper_tibble$bottomT[[1]] |> is.na()) {
          # Widen the search -- see if any neighboring cells are ocean-based. 
          ocean_found <- FALSE
          search_radius <- 1
          while(!ocean_found) {
            lon_indices <- (x_index - search_radius):(x_index + search_radius)
            lat_indices <- (y_index - search_radius):(y_index + search_radius)
            coper_search_tibble <- coper_data[,lon_indices,lat_indices,day] |> as_tibble()
            
            #' This logic isn't perfect!
            #' Search area is a square, thus may match with the non-closest point
            #' Search ignores position of ecomon within 1/12th degree central area
            #' This is unimportant for low land-matched coordinate counts and low search radius
            first_ocean_row <- which(complete.cases(coper_search_tibble))[1]
            
            if (is.na(first_ocean_row)) { # ocean points not yet found
              # widen the search radius
              search_radius <- search_radius + 1
            } else { # ocean points found!
              ocean_found <- TRUE
              coper_tibble <- coper_search_tibble[first_ocean_row,]
            }
          }
          
          # Print out some information
          print(paste0("Land-matched coordinate! ", ..., 
                       ". Final search radius: ", search_radius))
        }
        
        coper_tibble
      }
      
      # iterate through each point, grab the stars data
      coper_date_group <- date_group |>
        pmap_dfr(retrieve_coper_tibble)
      
      date_group |>
        select(-y_index, -x_index) |>
        cbind(coper_date_group |>
                select(-time) |>
                rename(coper_lon = x, coper_lat = y))
    }
    
    # Process day-by-day and then collapse into one big dataframe
    res <- seq_along(abund_groups) |> 
      lapply(match_date) |> 
      bind_rows()
    
    rm(coper_data, coper_db)
    
    res
  }
  
  date_split |>
    lapply(match_date_chunk) |>
    bind_rows()
}

jelly_results <- jelly_raw |>
  match_abundance_physical() |>
  rename(coel_m2 = ind_m2)

cfin_results <- cfin_raw |>
  match_abundance_physical() |>
  rename(corrected_cfin_m2 = ind_m2)
  
## Saving to file
root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus"

readr::write_csv(jelly_results,
                 file = file.path(root, 
                                  "input_data/matched_jellyfish_copernicus_physical_chfc.csv.gz"))

readr::write_csv(cfin_results, 
                 file = file.path(root, "input_data/matched_ecomon_copernicus_physical_chfc.csv.gz"))
