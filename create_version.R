species <- "coelenterates"
source("setup.R")
# Creation
source("data_preparation/data_from_config.R")
source("build_workflows.R")
# Analyses
source("model_analyses/analysis_functions_main.R")
source("generate_prediction_cubes.R")

# Assumes yaml_maker.R has already been run
v <- "coel.1.00"
config <- read_config(v)
if (config$training_data$species != species) {
  stop("Species of version is not the same as session defined species.")
}
set.seed(config$model$seed)

# Retrieve data
mec <- data_from_config(config)
dplyr::count(mec, patch) |> mutate(prop = n/sum(n))

# mec_split <- initial_split(mec, prop = 3/4, strata = patch)
K = 25 # 25 for any scale model 
mec_folds <- mec |>
  mc_cv(prop = .75, times = K, strata = patch)

# Creating workflow object & performance summary
mec_wkfs <- init_v_wkf(config, mec_folds)

### Analyses
testing <- get_v_testing(v)
threshold <- config$training_data$species_data$threshold$pre

# Calls to analysis functions
response_curves(v, mec_wkfs, mec, same_y = FALSE, save = TRUE)

auc_by_month(v, testing, save = TRUE)

threshold_vs_performance(v, testing, save = TRUE)

pred_vs_abund(v, testing, threshold, save = TRUE)

input_data_distribution(v)

