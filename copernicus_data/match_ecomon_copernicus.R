library(copernicus)
library(andreas)
library(stars)
library(dplyr)
library(ecomon)
library(purrr)

# Defining a region for our study -- this is pulled from nwa copernicus
bbox <- list(xlim = c(-77.0, -42.5), ylim = c(36.5, 56.66667))

########### Read in ecomon and copernicus datasets

# Vertically corrected ecomon data
emon_vc <- readr::read_csv(
  "/mnt/ecocast/projectdata/calanusclimate/src/vertical_correction_ecomon.csv.gz",
  col_types = readr::cols()) |>
  # Filter out records not within copernicus region, removes 487 records
  filter(latitude >= bbox$ylim[[1]], latitude <= bbox$ylim[[2]],
         longitude >= bbox$xlim[[1]], longitude <= bbox$xlim[[2]])
# Raw ecomon data
# ecomon_raw <- ecomon::read_staged(species = "calfin", form = "tibble")
emon_dates <- emon_vc$date |> unique()

# Retrieve copernicus physics data using andreas
coper_path <- copernicus_path("nwa/GLOBAL_MULTIYEAR_PHY_001_030")
coper_DB <- coper_path |> read_database()
# Filtering so that dates for copernicus and ecomon are identical, crucial for later matching
coper_db <- coper_DB |> filter(date %in% emon_dates)
paste("Date range:", min(coper_db$date), "to", max(coper_db$date))

coper_data <- read_andreas(coper_db, coper_path) # takes a second
coper_data[1,,,1] |> plot() # Plotting, no land area included

########### Match copernicus variables to ecomon records

#' this code works by relying on the makeup of a stars object. The points in 
#' the coper_data star raster are assigned by each 12th of a degree, so we can match ecomon
#' coordinates to the nearest twelfth of a degree to deduce the corresponding index
#' in coper_data. Because we extract the copernicus records only matching existing dates
#' in the ecomon dataset, we can then iterate through both objects day-by-day and extract
#' the copernicus information for ecomon records. 
#' 
#' Assumptions: 
#' - The set of dates in coper_data exactly matches ecomon's
#' - Coper data uses regular st_dimensions with offset -- NOT curvilinear, compare to brickman
#' - All ecomon data points are within the bounds of coper_data
#' - All points are located in the ocean
#' 
#' Existing issues: Some data points may match to land-based Copernicus, in which
#' case breadth-based search identifies a nearby ocean-based point. This algorithm 
#' should NOT be used if there are many land-matched coordinates, far-inland coordinates,
#' or mismatched coordinates right at the edge of the available dataset.
#' Could be target for a later fix, if necessary.

coper_x <- st_get_dimension_values(coper_data, 'x', center = TRUE)
coper_y <- st_get_dimension_values(coper_data, 'y', center = TRUE)
res_inv_x <- round(1/(coper_x[[2]] - coper_x[[1]])) # 12
res_inv_y <- round(1/(coper_y[[2]] - coper_y[[1]])) # -12

#' Retrieves nearest index value of an x or y value in coper_data based on 
#' resolution and start value
#' @param val dbl, value to convert
#' @param frac int, inverse of fraction resolution, e.g. 1/12° -> 12
#' @param start dbl, first value in copernicus data
#' @returns index position of nearest matching x/y coordinate value in coper_data
get_xy_index <- function(val, 
                         frac = c(res_inv, -res_inv)[[1]],
                         start = c(coper_x[[1]], coper_y[[1]])[[1]]) {
  
  diff_from_start <- val - start
  # index will be how many 1/frac it is from start (plus one)
  round(diff_from_start * frac) + 1
}

emon_mod <- emon_vc |>
  select(latitude, longitude, date, corrected_CIV_CVI_m2) |>
  filter(date >= min(coper_db$date), date <= max(coper_db$date)) |>
  mutate(lon_index = get_xy_index(longitude, frac = res_inv_x, start = coper_x[[1]]),
         lat_index = get_xy_index(latitude, frac = res_inv_y, start = coper_y[[1]])) 

# Sift our way through copernicus day by day to reduce memory usage
emon_groups <- emon_mod |> group_by(date) |> group_split()

#' Test that the indices of groups in emon_groups match indices of dates in coper_dates
#' This is guaranteed to hold true in this file by the process to extract coper_dates
# coper_dates <- st_get_dimension_values(coper_data, 'time') |> as.Date()
# 1:length(emon_groups) |> 
#   sapply(function(i) {emon_groups[[i]]$date[[1]] == coper_dates[[i]]}) |>
#   all()

#' Helper that retrieves the copernicus records for all ecomon records on a specific day
#' @param day int, the index of the date to retrieve
#' @returns df, ecomon lat/lon/date/abundance and matched copernicus lat/lon/env values
match_date_records <- function(day, species_data_groups = emon_groups) {
  date_group <- species_data_groups[[day]]
  
  #' Retrieves ocean copernicus data based on lon and lat indices
  #' Also handles instances where land-based match is made by accident
  retrieve_coper_tibble <- function(lon_index, lat_index, ...) {
    coper_tibble <- coper_data[,lon_index, lat_index, day] |> as_tibble()
    
    # In rare instances, the closest copernicus point might be an NA land-based value
    # This is basically the same thing as bfs_mismatched_indices() in setup.R
    if (coper_tibble$bottomT[[1]] |> is.na()) {
      # Widen the search -- see if any neighboring cells are ocean-based. 
      ocean_found <- FALSE
      search_radius <- 1
      while(!ocean_found) {
        lon_indices <- (lon_index - search_radius):(lon_index + search_radius)
        lat_indices <- (lat_index - search_radius):(lat_index + search_radius)
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
    select(-lat_index, -lon_index) |>
    cbind(coper_date_group |>
            select(-time) |>
            rename(coper_lon = x, coper_lat = y))
}

# Process day-by-day and then collapse into one big dataframe
matched_emon_coper <- seq_along(emon_groups) |> 
  lapply(match_date_records) |> 
  bind_rows()

# Any NA values? 1 identified
NA_rows <- which(!complete.cases(matched_emon_coper))
matched_emon_coper <- matched_emon_coper[-NA_rows,]

############# Save to file

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus"

readr::write_csv(matched_emon_coper,
                 file = file.path(root, "input_data/matched_ecomon_copernicus_physical_nwa.csv.gz"))

# Examine values that were matched to land-based coordinates in original code
if (FALSE) {
  #### Examine missing values, if any
  missing_indices <- which(matched_emon_coper$bottomT |> is.na())
  # Three values are missed by the original calculations
  missing_values <- matched_ecomon_coper[missing_indices,]
  plot_gen(missing_values, "corrected_CIV_CVI_m2", "Missing Copernicus Values", 4)
  
  missing_test_tibble <- missing_values |>
    select(latitude, longitude, date, corrected_CIV_CVI_m2) |>
    mutate(lon_index = get_xy_index(longitude, frac = res_inv_x, start = coper_x[[1]]),
           lat_index = get_xy_index(latitude, frac = res_inv_y, start = coper_y[[1]]))
  
  missing_groups <- missing_test_tibble |> group_by(date) |> group_split()
  
  matched_missing_groups <- seq_along(missing_groups) |> 
    lapply(function(x) match_date_records(x, missing_groups)) |> 
    bind_rows()
}

