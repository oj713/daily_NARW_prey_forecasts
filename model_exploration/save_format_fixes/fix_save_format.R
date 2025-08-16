species <- "cfin"
source("setup.R")
source("generate_data_cubes.R")

v <- "cf.0.00"

#### Testing file sizes for a whole year's worth of data
folder <- v_path(v, "preds", "3day_resolution_3fold")
stars1993 <- read_quantile_stars(file.path(folder, "1993"))

write_mdim(stars1993, file.path(folder, "stars1993_float.nc"), as_float = TRUE)


testdates <- seq(as.Date("2015-07-15"), by = "days", length.out = 25)

saveable_res <- generate_prediction_cubes(v, 
                                          testdates, 
                                          save_folder = NULL, 
                                          fold_subset = 3, 
                                          verbose = TRUE)
saveable_res

float_nc <- "model_exploration/save_format_fixes/out_asfloat.nc"
dbl_nc <- "model_exploration/save_format_fixes/out_asdbl.nc"

write_mdim(saveable_res, float_nc, as_float = TRUE)
write_mdim(saveable_res, dbl_nc, as_float = FALSE)

read_float <- read_mdim(float_nc)
lon <- st_get_dimension_values(read_float, "lon", where = "start")
lat <- st_get_dimension_values(read_float, "lat", where = "start")

read_float <- st_set_dimensions(read_float, "lon", values = round(lon * 24)/24)
read_float <- st_set_dimensions(read_float, "lat", values = round(lat * 24)/24)

#' Recovers interval dimensions for a stars object with numeric dimensions
#' that didn't cleanly convert from fractions to floats
#' @param stars_obj stars, stars object
#' @param true_offset dbl, absolute value of true offset. Always 1/12° for copernicus
#' @return stars, stars object with appropriate dimensions fixed according to true_offset
recover_interval_dims <- function(stars_obj, true_offset = 1/12) {
  # If offset is present for all variables then no fix is needed
  needs_fixing <- data.frame(st_dimensions(stars_obj))$offset == "NA"
  if (!any(needs_fixing)) {return(stars_obj)}
  
  # Extract values of dimensions that 1) need to be fixed and 2) are numeric
  dims_values <- expand_dimensions(stars_obj, center = FALSE)[needs_fixing]
  dims_values <- dims_values[sapply(dims_values, class) == "numeric"]
  
  for (dim_name in names(dims_values)) {
    # Round values to the nearest (true_offset/2)th, dividing by 2 due to center behavior
    fixed_values <- round(dims_values[[dim_name]]/(true_offset/2)) * true_offset/2
    # Update dimension of stars_obj accordingly
    stars_obj <- st_set_dimensions(stars_obj, dim_name, values = fixed_values)
  }
  
  stars_obj
}



### Borken
broken_saveable <- read_mdim(test_file_path_nc)



saveable_dup <- saveable_res

st_get_dimension_values(saveable_dup, "lon") <- 
  st_dimension_values(saveable_dup, "lon") |> as.vector()


