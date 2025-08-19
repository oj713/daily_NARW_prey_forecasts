setwd("/mnt/ecocast/projects/students/ojohnson/daily-forecasts")
print(getwd())
species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")
source("generate_prediction_cubes.R")

v <- "je.0.00"
config <- read_config(v)
wkfs <- get_v_wkfs(v)
n_folds <- length(wkfs)

data <- generate_covariate_cube(config, as.Date(c("2015-07-14", "2015-07-16")))

#' Calculates time taken to do a thing, average of three runs
#' basically system.time with printing
timeTaken <- function(functionToRun) {
  oneIter <- function(n) {
    cat("\r",  n, "/ 3 ...")
    starttime <- Sys.time()
    functionToRun()
    as.numeric(Sys.time() - starttime)
  }
  1:3 |> sapply(oneIter) |> mean()
}

# No parallel, calling predictable subset off the bat
# 30.876
orig <- function() {
  # Calculates predictions from a single workflow
  predictable_indices <- complete.cases(data)
  
  get_wkf_column <- function(wkf, idx) {
    res <- predict(wkf, data[predictable_indices,], type = "prob") |>
      select(.pred_1)
    
    if (TRUE) {
      cat("\r Predicting...", idx, "/", n_folds)
    }
    
    res
  }
  
  wkf_preds <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages()
}

# Same as original, but partitioning data by predictableIndices one time
# 30.425, little bit faster! 
orig_oneSubset <- function() {
  # Calculates predictions from a single workflow
  predictable_indices <- complete.cases(data)
  predictable_data <- data[predictable_indices,]
  
  get_wkf_column <- function(wkf, idx) {
    res <- predict(wkf, predictable_data, type = "prob") |>
      select(.pred_1)
    
    if (TRUE) {
      cat("\r Predicting...", idx, "/", n_folds)
    }
    
    res
  }
  
  wkf_preds <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages()
}

# The recipe is the same for all models // extract and bake once
# 29.8912 Litte faster! 
oneS_splitoutRecipe <- function() {
  wkf_rec <- extract_preprocessor(wkfs[[1]]) |> prep()
  predictable_indices <- complete.cases(data)
  id_vars <- wkf_rec$term_info |>
    filter(role == "ID") |>
    pull(variable)
  predictable_data <- wkf_rec |>
    bake(new_data = data[predictable_indices,]) |>
    select(-all_of(id_vars))
  
  models <- map(wkfs, extract_fit_parsnip)
  
  get_mod_column <- function(mod, idx) {
    res <- predict(mod, predictable_data, type = "prob") |>
      select(.pred_1)
    
    if (TRUE) {
      cat("\r Predicting...", idx, "/", n_folds)
    }
    
    res
  }
  
  wkf_preds <- models |>
    imap(get_mod_column) |>
    bind_cols() |>
    suppressMessages()
}

# Add parallel processing
library(furrr)

preptime <- Sys.time()
wkf_rec <- extract_preprocessor(wkfs[[1]]) |> prep()
predictable_indices <- complete.cases(data)
id_vars <- wkf_rec$term_info |>
  filter(role == "ID") |>
  pull(variable)
predictable_data <- wkf_rec |>
  bake(new_data = data[predictable_indices,]) |>
  select(-all_of(id_vars))

models <- map(wkfs, extract_fit_parsnip)

preptime <- as.numeric(Sys.time() - preptime)

## MULTISESSION : 10.236
plan(multisession, workers = 4)

cat("|", paste(rep(" ", n_folds), collapse = ""), "| Predicting...")

oneS_splitoutRecipe <- function() {
  get_mod_column <- function(mod, idx) {
    cat("\r|", paste(rep("+", idx), collapse = ""))
    predict(mod, predictable_data, type = "prob") |>
      dplyr::select(.pred_1)
  }
  
  wkf_preds <- models |>
    furrr::future_imap(get_mod_column,
                       .options = furrr_options(seed = config$model$seed)) |>
    bind_cols() |>
    suppressMessages()
  
  cat("\n")
}

oneS_splitoutRecipe()

# res <- timeTaken(oneS_splitoutRecipe) + preptime
# print(res)

plan(sequential)

