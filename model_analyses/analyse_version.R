species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")
source("model_analyses/analysis_functions_main.R")

v <- "je.0.00"
v_config <- read_config(v)
wkfs <- get_v_wkfs(v)
testing <- get_v_testing(v)
vdata <- data_from_config(v_config)
threshold <- v_config$training_data$species_data$threshold$pre

#### Calls to analysis functions

response_curves(v, wkfs, vdata, same_y = FALSE, save = TRUE)
  
auc_by_month(v, testing, save = TRUE)

threshold_vs_performance(v, testing, save = TRUE)

# don't forget to alter the threshold if it's different!
pred_vs_abund(v, testing, threshold, save = TRUE)