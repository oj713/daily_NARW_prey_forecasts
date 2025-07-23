species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")

# Assumes yaml_maker.R has already been run
v <- "je.0.00"
config <- read_config(v)
if (config$training_data$species != species) {
  stop("Species of version is not the same as session defined species.")
}
set.seed(config$model$seed)

# Retrieve data
mec <- data_from_config(config)

