setwd("/mnt/ecocast/projects/students/ojohnson/daily-forecasts")
print(getwd())
species <- "coelentrates"
source("setup.R")
source("data_preparation/data_from_config.R")
source("generate_prediction_cubes.R")

v <- "coel.0.01"
config <- read_config(v)
wkfs <- get_v_wkfs(v)
n_folds <- length(wkfs)

data <- generate_covariate_cube(config, 
                                seq(as.Date("1994-01-01"), by = "days", length.out = 90))

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
# 30.876 [2 dates]
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
# 30.425, little bit faster! [2 dates]
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
# 29.8912 Litte faster! [2 dates]
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

# Basic parallel processing, 10.236 [2 dates] but breaks for large sizes
if (FALSE) {
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

res <- timeTaken(oneS_splitoutRecipe) + preptime
print(res)

plan(sequential)
}

library(furrr)
library(tictoc)

#' Helper: extracts recipe & applies to data
get_predictable_data <- function() {
  predictable_indices <- complete.cases(data)
  
  # The recipe is the same across all workflows: therefore, we can 
  # bake the data just once to save processing time
  wkf_rec <- extract_preprocessor(wkfs[[1]]) |> prep()
  id_vars <- filter(wkf_rec$term_info, role == "ID")$variable 
  wkf_rec |>
    bake(new_data = data[predictable_indices,]) |>
    select(-all_of(id_vars))
}

#' Helper: executes parallel processing command within trycatch environment
test_parallel <- function(fn) {
  tryCatch({
    # Bump up maximum passable dataset size
    options(future.globals.maxSize = 1.0 * 1e9)  
    plan(multisession, workers = 4)
    tic()
    fn()
  },
  error = function(e) {
    message("\nDud...", e)
    e
  },
  finally = { # Make sure parallel is turned off, regardless of error or not!
    toc()
    plan(sequential)
    options(future.globals.maxSize = 500 * 1024 ^ 2) ## 500 MiB, default
  })
}

# Breaks
res_original <- test_parallel(function() {
  apply_quantile_preds(wkfs, data, verbose = TRUE)
})

# env_globals off
# 388.952 seconds elapsed
env_globals_off <- function() {
  pd <- get_predictable_data()
  models <- map(wkfs, extract_fit_parsnip)
  cat("\n|", paste(rep(" ", n_folds), collapse = ""), "| Predicting...")
  
  #' Helper: Calculates predictions from a single workflow
  get_mod_column <- function(mod, idx) {
    cat("\r|", paste(rep("+", idx), collapse = ""))
    predict(mod, pd, type = "prob") |>
      dplyr::select(.pred_1)
  }
  models |>
    furrr::future_imap(get_mod_column,
                       .options = furrr_options(seed = 1, globals = FALSE)) |>
    bind_cols() |>
    suppressMessages()
  
}
res_env_globals_off <- test_parallel(env_globals_off)

# Map over data chunks rather than workers
# Couldn't get this one to work
res_map_datachunks <- test_parallel(function() {
  pd <- get_predictable_data()
  pd_chunked <- split(pd, rep(1:4, length.out = nrow(pd)) |> sort() |> factor())
  
  models <- map(wkfs, extract_fit_parsnip)
  cat("\n|", paste(rep(" ", n_folds), collapse = ""), "| Predicting...")
  
  #' Helper: Calculates predictions from a single workflow
  predict_data_chunk <- function(pd_chunk) {
    models |>
      map(~predict(.x, pd_chunk, type = "prob") |>
            dplyr::select(.pred_1)) |>
      dplyr::bind_cols() |>
      suppressMessages()
  }
  pd_chunked |>
    furrr::future_map(predict_data_chunk,
                      .options = furrr_options(seed = 1, globals = FALSE)) |>
    bind_rows()
})

