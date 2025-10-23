species <- ""
source("setup.R")
source("data_preparation/data_from_config.R")
source("build_workflows.R")

# Comment out or add items to this list as desired.
versions_to_predict <- list(
  list("spec" = "pseudocalanus", "v" = "pseu.0.01"),
  list("spec" = "centropages", "v" = "cent.0.01"),
  list("spec" = "cfin", "v" = "cfin.0.01"),
  list("spec" = "salpa", "v" = "salp.1.00")
  #list("spec" = "coelenterates", "v" = "coel.1.00"),
  #list("spec" = "siphonophora", "v" = "siph.1.00")
)

for (vspec in versions_to_predict) {
  gc()
  
  species <- vspec$spec
  cat("\n Kicking off species...", species)
  root <- get_root(species)
  v <- vspec$v
  print(species)
            
  config <- read_config(v)
  
  mec <- data_from_config(config)
  
  test_recipe <- workflow_from_config(config, mec) |>
    extract_preprocessor()
  
  xgb_tunable <-  boost_tree(
    trees = tune(), ## model complexity
    tree_depth = tune(), 
    min_n = 10, 
    #loss_reduction = tune(),              
    #sample_size = tune(),  ## randomness
    mtry = tune(), 
    learn_rate = tune(),  ## step size
  ) |>
    set_engine("xgboost") |> 
    set_mode("classification")
  
  folds <- vfold_cv(mec, v = 5, repeats = 1, strata = patch)
  
  ## Standard tuning  
  workflow <- workflow(preprocessor = test_recipe, spec = xgb_tunable)
  
  numfeatures <- ncol(mec) - 5
  
  xgb_grid <- grid_regular(
    trees(c(500, 1000)),
    tree_depth(c(2, 6)),
    mtry(c(round(numfeatures/4), round(numfeatures * .75))),
    #loss_reduction(c(0,)),
    #sample_size = sample_prop(c(.7, .9)),
    learn_rate(c(-3, -1)),
    levels = 2
  )
  
  res <- tune_grid(
    workflow,
    resamples = folds,
    grid = xgb_grid,
    metrics = metric_set(roc_auc, accuracy, sens, spec, f_meas)
  )
  
  p <- autoplot(res, metric = "f_meas") + 
    ggtitle(paste(species, "tuning results, F1 Score"))
  p2 <- autoplot(res, metric = "roc_auc") + 
    ggtitle(paste(species, "tuning results, ROC AUC"))
  
  pdf(v_path(v, "model", "tuning_results.pdf"))
  print(list(p, p2))
  dev.off() 
}  

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