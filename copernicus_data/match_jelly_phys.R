source("setup.R")

library(ecomon)
library(andreas)
ecomon <- ecomon::scale_ecomon()

## Will need to modify this code since the size of coper_data is too large given
# the higher number of date records 

jellydata <- select(ecomon, c(lat, lon, date, depth, coel_m2))

# Defining a region for our study -- this is pulled from nwa copernicus
bbox <- list(xlim = c(-77.0, -42.5), ylim = c(36.5, 56.66667))

# removes 1000 records
jd_filtered <- jellydata |>
  filter(lat >= bbox$ylim[[1]], lat <= bbox$ylim[[2]],
         lon >= bbox$xlim[[1]], lon <= bbox$xlim[[2]])

emon_dates <- jd_filtered$date |> unique()

# Retrieve copernicus physics data using andreas
coper_path <- copernicus_path("nwa/GLOBAL_MULTIYEAR_PHY_001_030")
coper_DB <- coper_path |> read_database()
# Filtering so that dates for copernicus and ecomon are identical, crucial for later matching
coper_db <- coper_DB |> filter(date %in% emon_dates)
paste("Date range:", min(coper_db$date), "to", max(coper_db$date))

coper_data <- read_andreas(coper_db, coper_path) # takes a second
coper_data[1,,,1] |> plot() # Plotting, no land area included

coper_x <- st_get_dimension_values(coper_data, 'x', center = TRUE)
coper_y <- st_get_dimension_values(coper_data, 'y', center = TRUE)
res_inv_x <- round(1/(coper_x[[2]] - coper_x[[1]])) # 12
res_inv_y <- round(1/(coper_y[[2]] - coper_y[[1]])) # -12

get_xy_index <- function(val, 
                         frac = c(res_inv, -res_inv)[[1]],
                         start = c(coper_x[[1]], coper_y[[1]])[[1]]) {
  
  diff_from_start <- val - start
  # index will be how many 1/frac it is from start (plus one)
  round(diff_from_start * frac) + 1
}

jd_mod <- jd_filtered |>
  select(lat, lon, date, coel_m2) |>
  filter(date >= min(coper_db$date), date <= max(coper_db$date)) |>
  mutate(lon_index = get_xy_index(lon, frac = res_inv_x, start = coper_x[[1]]),
         lat_index = get_xy_index(lat, frac = res_inv_y, start = coper_y[[1]])) 

# Sift our way through copernicus day by day to reduce memory usage
jd_groups <- jd_mod |> group_by(date) |> group_split()

# go load match_date_records from match_ecomon_copernicus.R
# Process day-by-day and then collapse into one big dataframe
matched_emon_coper <- seq_along(jd_groups) |> 
  lapply(match_date_records) |> 
  bind_rows()



