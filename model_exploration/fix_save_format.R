species <- "cfin"
source("setup.R")
source("generate_data_cubes.R")

v <- "cf.0.00"

testdates <- seq(as.Date("2015-07-15"), by = "days", length.out = 5)

saveable_res <- generate_prediction_cubes(v, 
                                          testdates, 
                                          save_folder = NULL, 
                                          fold_number = 5, 
                                          verbose = TRUE)
saveable_res

test_file_path_nc <- "model_exploration/testsave_deleteme.nc"

write_mdim(saveable_res, test_file_path_nc)

### Borken
broken_saveable <- read_mdim(test_file_path_nc)



saveable_dup <- saveable_res

st_get_dimension_values(saveable_dup, "lon") <- 
  st_dimension_values(saveable_dup, "lon") |> as.vector()


