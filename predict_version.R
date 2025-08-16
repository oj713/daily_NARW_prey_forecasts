species <- "cfin"
source("setup.R")
source("generate_prediction_cubes.R")

v <- "cf.0.01"

date_start = as.Date("1993-01-01")
date_end = as.Date("2019-12-31")
#date_end = as.Date("1993-8-31")

res <- generate_yearly_cubes(v, 
                             date_start, 
                             date_end, 
                             date_downsample = 2,
                             fold_subset = 15,
                             add = TRUE,
                             verbose = TRUE, 
                             as_float = FALSE)

## Test that things are working alright before running the final version
if (FALSE) {
dates_test <- 
  list("summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2015/9/20", "2015/10/20")),
       "winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20")))

res <- generate_prediction_cubes(v, 
                                 dates_test, 
                                 save_folder = "test_predset", 
                                 verbose = TRUE, 
                                 max_chunk_size = 2,
                                 fold_subset = 5, 
                                 add = FALSE)
}