species <- "jellyfish"
source("setup.R")
source("generate_data_cubes.R")

v <- "je.0.00"

date_start = as.Date("1993-01-01")
date_end = as.Date("2025-02-28")
date_end = as.Date("1993-12-31")

generate_yearly_cubes(v, 
                      date_start, 
                      date_end, 
                      verbose = TRUE)

if (FALSE) {
dates_testing <- list("winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20")),
                      "summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2015/09/20")))
generate_prediction_cubes(v, 
                          dates_nested, 
                          save_folder = "test_chunking1", 
                          verbose = TRUE, 
                          max_chunk_size = 3)
}