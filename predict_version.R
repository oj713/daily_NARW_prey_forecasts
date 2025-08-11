species <- "jellyfish"
source("setup.R")
source("generate_prediction_cubes.R")

v <- "je.0.00"

date_start = as.Date("1993-01-01")
date_end = as.Date("2019-11-11")
#date_end = as.Date("1993-8-31")

res <- generate_yearly_cubes(v, 
                             date_start, 
                             date_end, 
                             date_downsample = NULL,
                             fold_subset = 2,
                             add = TRUE,
                             verbose = TRUE)

if (FALSE) {
dates_deliberatechunkbreak <- 
  list("summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2015/9/20", "2035/10/20")),
       "winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20")))
dates_fix_chunk_break <- 
  list("summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2015/9/20", "2015/10/20")),
       "winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20")))

res <- generate_prediction_cubes(v, 
                                 dates_deliberatechunkbreak, 
                                 save_folder = "test_delibratechunkbreak", 
                                 verbose = TRUE, 
                                 max_chunk_size = 2,
                                 fold_subset = 3, 
                                 add = TRUE)
}