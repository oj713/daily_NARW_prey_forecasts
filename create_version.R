species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")
source("build_workflows.R")

# Assumes yaml_maker.R has already been run
v <- "je.0.00"
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
