setwd("/mnt/ecocast/projects/students/ojohnson/daily-forecasts")
print(getwd())
species <- "coelentrates"
source("setup.R")
source("data_preparation/data_from_config.R")
source("generate_prediction_cubes.R")

# Setup code, run once
if (FALSE) {
  library(tictoc)
  library(future)
  library(furrr)
  
  v <- "coel.0.01"
  config <- read_config(v)
  wkfs <- get_v_wkfs(v)
  n_folds <- 25
  
  data <- generate_covariate_cube(config, 
                                  seq(as.Date("1994-01-01"), by = "days", length.out = 2))
  
  ### Parallel processing workflow
  test_multisession <- function(fn) {
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
}

## Let's try the strategy where we save each individual model to disk...
if (FALSE) {
models <- wkfs |> map(extract_fit_parsnip)
model_path <- v_path(v, "model", "ind_models")
dir.create(model_path)
save_ind_model <- function(mod, idx) {
  saveRDS(mod, file.path(model_path, paste0("mod", idx, ".csv.gz")))
}
imap(models, save_ind_model)
}

# 13.635 seconds [2 dates]
res <- test_multisession(function() {
  predictable_matrix <- get_predictable_data() |>
    as.matrix()
  cat("\n|", paste(rep(" ", n_folds), collapse = ""), "| Predicting...")
  
  models <- map(wkfs, ~extract_fit_parsnip(.x)$fit |>
                  xgboost::xgb.Booster.complete())
  
  #' Helper: Calculates predictions from a single workflow
  get_mod_column <- function(mod, idx, mat) {
    cat("\r|", paste(rep("+", idx), collapse = ""))
    
    predict(mod, xgboost::xgb.DMatrix(mat))
  }
  res <- models |>
    furrr::future_imap(~get_mod_column(.x, .y, predictable_matrix), 
              .options = furrr_options(seed = 5, globals = FALSE))
  Reduce(rbind, res)
})



