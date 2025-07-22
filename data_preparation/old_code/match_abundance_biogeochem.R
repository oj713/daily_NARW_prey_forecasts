source("old_setup.R")

jelly_phys <- get_input_data("matched_jellyfish_copernicus_physical_chfc.csv.gz") |>
  rename(ind_m2 = coel_m2)

#' Matches abundance records in a dataset to copernicus biogeochemical values.
#' As opposed to match_abundance_physical, reads in date-by-date rather than in date
#' chunks due to larger file sizes
#' @param abund_data df, abundance values. Required columns lon, lat, date, ind_m2
#' @return df, matched abundance records with original columns + physical covariates
match_abundance_bgc <- function(abund_data) {
  # biogeochemical info
  coper_info <- get_coper_info(region = "world", type = "bgc")
  
  # Cropping data to match spatial and temporal dims of copernicus
  abund_data <- abund_data |>
    mutate(x_index = coper_info$get_xy_index(lon, axis = "x"),
           y_index = coper_info$get_xy_index(lat, axis = "y"))
  
  # How many dates are represented by these points?
  full_date_vec <- abund_data$date |> unique()
  
  #' Helper which matches all records for a date to copernicus information
  #' @param d Date
  #' @return df, abund_data records for date matched to copernicus biogeochem values
  match_single_date <- function(d) {
    ## Filter to date
    coper_db <- coper_info$meta_db |> filter(date == d)
    coper_data <- read_andreas(coper_db, coper_info$coper_path) 
    
    abund_date <- jelly_phys |> filter(date == d)
    
    #' Retrieves the data row corresponding to a coordinate
    retrieve_coper_tibble <- function(x_index, y_index, ...) {
      coper_tibble <- coper_data[, x_index, y_index] |> as_tibble()
      
      # In rare instances, the closest copernicus point might be an NA land-based value
      # This is basically the same thing as bfs_mismatched_indices() in setup.R
      if (coper_tibble$chl[[1]] |> is.na()) {
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
    
    coper_date_group <- abund_date |>
      pmap_dfr(retrieve_coper_tibble)
    
    returnval <- abund_date |>
      select(-y_index, -x_index) |>
      cbind(coper_date_group |> select(-x, -y))
    
    rm(coper_data)
    
    return(returnval)
  }
  
  full_date_vec |>
    lapply(match_single_date) |>
    bind_rows()
}

jelly_phys_bgc <- jelly_phys |>
  match_abundance_bgc() |>
  rename(coel_m2 = ind_m2)

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus"
readr::write_csv(jelly_phys_bgc,
                 file = file.path(root, 
                                  "input_data/matched_jellyfish_copernicus_phys_bgc_chfc.csv.gz"))


