cat("Kicking off predictions ...\n")
setwd("/mnt/ecocast/projects/students/ojohnson/daily-forecasts")

versions_to_predict <- list(
  list("spec" = "salpa", "v" = "salp.0.00"),
  list("spec" = "siphonophora", "v" = "siph.0.00")
)

for (vspec in versions_to_predict) {
  species <- vspec$spec
  cat("\n Kicking off species...", species)
  source("setup.R")
  source("generate_prediction_cubes.R")
  
  v <- vspec$v
  verbose <- TRUE
  date_start = as.Date("1993-01-01")
  date_end = as.Date("2019-12-31")
  
  res <- generate_yearly_cubes(v, 
                               date_start, 
                               date_end, 
                               date_downsample = NULL,
                               fold_subset = NULL,
                               add = TRUE,
                               verbose = verbose, 
                               as_float = TRUE)
  
  consolidate_preds_monthly(v, verbose = verbose, as_float = FALSE)
  
  
  gc()
}

## Test that things are working alright before running the final version
if (FALSE) {
dates_test <- 
  list("summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2015/9/20", "2015/10/20")),
       "winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20", "2015/4/20")))
dates_deliberatebreak <- 
  list("summer" = as.Date(c("2014/5/20", "2015/6/20", "2015/7/20", "2015/8/20", "2035/9/20", "2035/10/20")),
       "winter" = as.Date(c("2014/11/20", "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20")))

# v.0.01 = 84 sec when nthread = 1, 31 sec when nthread = 4
res <- generate_prediction_cubes(v, 
                                 dates_test, 
                                 save_folder = "example_2015", 
                                 verbose = TRUE, 
                                 max_chunk_size = 74,
                                 fold_subset = NULL, 
                                 add = TRUE)

d <- read_quantile_stars(v_pred_path(v, "summer", "example_2015"))
ggplot() +
  geom_stars(data = d["50%",,,]) + 
  facet_wrap("date") + 
  scale_fill_viridis(na.value = "transparent") + 
  theme_bw() + 
  coord_quickmap()
}