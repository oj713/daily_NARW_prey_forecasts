source("setup.R")

jec <- get_input_data("jelly_coper_all_revised.csv.gz")

jec <- jec |>
  mutate(presence = (ind_m2 > 0) |> factor(), 
         vel = sqrt(uo^2 + vo^2),
         month = lubridate::month(date) |> factor(levels = 1:12))

jec_split <- initial_split(jec, prop = .75, strata = presence)
folds <- vfold_cv(training(jec_split), v = 5, repeats = 1, strata = presence)

# Tuning
xgb_spec <- boost_tree(
  trees = tune(), ## model complexity
  tree_depth = tune(), 
  min_n = 10, 
  #loss_reduction = tune(),              
  #sample_size = tune(),  ## randomness
  mtry = 5, 
  learn_rate = tune(),  ## step size
) |>
  set_engine("xgboost") |> 
  set_mode("classification")
  
recipe_spec <- 
  recipe(presence ~ bathy_depth + vel + month + mlotst + bottomT + thetao + so +
           zos + chl + no3 + nppv + o2 + po4 + si,
         data = training(jec_split)) |>
  step_log(bathy_depth, offset = 1, base = 10) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

#### Testing correlated variable removal

recipe_alt <- 
  recipe(presence ~ bathy_depth + vel + month + mlotst + bottomT + thetao + so +
           zos + chl + no3 + nppv + si,
         data = training(jec_split)) |>
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

rec_list <- list(reg = recipe_spec, corr_removals = recipe_alt)
jelly_wkfs <- workflow_set(rec_list, list(model_spec), 
                          cross = TRUE)

fitted_wkfs <- jelly_wkfs |>
  workflow_map(fn = "fit_resamples", # method to apply in quotes
               verbose = FALSE, 
               seed = 400, 
               resamples = folds, # arguments for fn 
               metrics =  metric_set(roc_auc, sens, spec, accuracy),
               control = control_resamples(save_pred = TRUE, save_workflow = TRUE))

fitted_wkfs |>
  collect_metrics()

wkf_corr <- fitted_wkfs |>
  extract_workflow("corr_removals_boost_tree") |>
  fit(training(jec_split))
  
### Standard tuning  
workflow <- workflow(preprocessor = recipe_spec, spec = xgb_spec)
  
xgb_grid <- grid_regular(
    trees(c(500, 1000)),
    tree_depth(c(2, 6)),
    #loss_reduction(c(0,)),
    #sample_size = sample_prop(c(.7, .9)),
    learn_rate(c(-3, -1)),
    levels = 3
)
  
res <- tune_grid(
    workflow,
    resamples = folds,
    grid = xgb_grid,
    metrics = metric_set(roc_auc, accuracy, sens, spec)
)
  
best_res <- res |>
  show_best(metric = "roc_auc")
autoplot(res, metric = "roc_auc")
  
collect_metrics(res)
  
  
  # assumes train, workflow
get_splits <- function(params) {
  strings <- workflow |>
    finalize_workflow(params) |>
    fit(train) |>
    extract_fit_engine() |>
    xgb.dump(dump_format = "text")
  total_splits = 0
  for(tree_string in strings) {
    leaves = stringr::str_count(tree_string, "leaf")
    total_splits <- total_splits + 1 - leaves
  }
  total_splits
}
  
get_splits(dplyr::slice(best_res, 3) |>
              select(-(.metric:std_err)))