source("setup.R")

root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"
mec <- readr::read_csv(file.path(root, "mec_brickman_bathy.csv.gz"),
                       col_types = readr::cols())

abundance_threshold <- 30000

mec <- mec |>
  mutate(patch = (corrected_CIV_CVI_m2 >= abundance_threshold) |> factor(), 
         vel = sqrt(uo^2 + vo^2),
         month = lubridate::month(date) |> factor(levels = 1:12))

mec_split <- initial_split(mec, prop = .75, strata = patch)

### Create a workflow
recipe_spec <- 
  recipe(patch ~ bathymetry + vel + month + mlotst + bottomT + thetao + so,
         data = training(mec_split)) |>
  step_log(bathymetry, offset = 1, base = 10) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

model_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 500,
           learn_rate = .1,
           tree_depth = 4,
           mtry = 5,
           min_n = 10)
                      
workflow_template <- workflow() |>
  add_recipe(recipe_spec) |>
  add_model(model_spec)

fitted_wkf <- fit(workflow_template, training(mec_split))

## Evaluate the model

results <- augment(fitted_wkf, testing(mec_split))

my_metrics <- metric_set(roc_auc, sens, spec, accuracy)

fitted_wkf |>
  saveRDS("test_model_v0.csv.gz")

# my_metrics(results, truth = patch, estimate = .pred_class, .pred_TRUE, event_level = "second")

# roc_curve(results, truth = patch, .pred_FALSE) |> autoplot()
