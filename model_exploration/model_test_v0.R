source("setup.R")

mec <- get_input_data("mec_bathy_ET.csv.gz")

abundance_threshold <- 30000

mec <- mec |>
  mutate(patch = (corrected_CIV_CVI_m2 >= abundance_threshold) |> factor(), 
         vel = sqrt(uo^2 + vo^2),
         month = lubridate::month(date) |> factor(levels = 1:12))

mec_split <- initial_split(mec, prop = .75, strata = patch)

### Create a workflow
recipe_spec <- 
  recipe(patch ~ bathy_depth + vel + month + mlotst + bottomT + thetao + so,
         data = training(mec_split)) |>
  step_log(bathy_depth, offset = 1, base = 10) |>
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
  saveRDS("model_exploration/test_model_v0.csv.gz")
