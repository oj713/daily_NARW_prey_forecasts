##### Custom recipe steps to add to workflow, specified in yaml configuration

#' Logs bathymetry
step_log_bathy <- function(recipe) {
  step_log(recipe, bathy_depth, offset = 1)
}

#' Normalizes all numeric predictors
step_normalize_numeric <- function(recipe) {
  step_normalize(recipe, all_numeric_predictors())
}

##### Functions to initialize a version from config

#' Builds a workflow template from a yaml configuration
#' @param config list, yaml configuration for version
#' @param template_data df, template data for normalization step
#' @return untrained workflow
workflow_from_config <- function(config, template_data) {
  # Supported model types
  model_funcs <- list(
    "Boosted Regression Tree" = boost_tree,
    "MLP Neural Network" = mlp
  )
  if (!(config$model$model$name %in% names(model_funcs))) {
    stop("Model type ", config$model$model$name, " not yet supported.")
  }
  
  # Deriving roles from config
  predictor_vars <- config$training_data$coper_data |> unlist()
  names(predictor_vars) <- rep("predictor", times = length(predictor_vars))
  ID_vars <- colnames(template_data)[!(colnames(template_data) %in% c(predictor_vars, "patch"))]
  names(ID_vars) <- rep("ID", times = length(ID_vars))
  var_roles <- c("outcome" = "patch", ID_vars, predictor_vars)
  
  ## Initializing recipe
  recipe_spec <- recipe(template_data, 
                        vars = var_roles, 
                        roles = names(var_roles)) |>
    step_dummy(all_nominal_predictors()) # currently all supported models require dummys
  # For each transform in the config, call it
  for (transformation in config$model$transformations) {
    recipe_spec <- do.call(transformation, list(recipe_spec))
  }
  
  ## Initializing model based on config
  model_args <- config$model$model[!names(config$model$model) %in% c("name", "engine")]
  model_spec <- model_funcs[[config$model$model$name]]() |>
    set_mode("classification") |>
    set_engine(config$model$model$engine)
  model_spec <- do.call(set_args, c(list(object = model_spec), model_args))
  
  ## Returning workflow template
  workflow() |>
    add_recipe(recipe_spec) |>
    add_model(model_spec)
}

#' Calculates some basic performance statistics for a model
#' @param config list, yaml configuration for a version
#' @param testing_results df, testing results from init_v_wkf
#' @return list, list of relevant performance statistics 
get_performance_summary <- function(config, testing_results) {
  num_folds <- testing_results$wkf_id |> max()
  
  # Metrics to calculate
  basic_metrics <- metric_set(roc_auc, f_meas, spec, sens, accuracy)

  # Calculating for each individual fold
  quantiles <- testing_results |>
    group_by(wkf_id) |>
    group_map(~basic_metrics(.x, truth = patch, estimate = .pred_class, .pred_1) |>
                mutate(.y)) |>
    bind_rows() |>
    pivot_wider(id_cols = wkf_id, names_from = .metric, values_from = .estimate) |>
    select(-wkf_id) |>
    apply(2, function(col) quantile(col, probs = c(.05, .5, .95)))
  
  list_quantiles <- apply(quantiles, 2, function(x) list(x)) |>
    map(~.x[[1]] |> as.list() |> setNames(rownames(quantiles)))
  
  list(version = config$version, 
       num_folds = num_folds,
       performance_statistics = list_quantiles)
}

#' Builds and saves a workflow/workflowset to file
#' Also saves some basic summary stats to file
#' @param config list, yaml configuration for version
#' @param mec_folds rsplit or rset, training data for model
#' @return workflowset. saves wkfs to file + testing dataset + basic analyses
init_v_wkf <- function(config, mec_folds, verbose = TRUE) {
  
  # Defining workflow
  if (!("rset" %in% class(mec_folds))) {
    stop("This data is not an rset object.")
  }
  n_folds <- nrow(mec_folds)
  template_data <- mec_folds$splits[[1]] |> training()
  wkf_template <- workflow_from_config(config, template_data)
  
  if (verbose) { print("Starting training...") }
  
  # Prints percent progress, super helpful for longer training periods
  progressive_fit <- function(fold, idx) {
    if (verbose) {
      cat("\r", round((idx - 1)/n_folds, 2) * 100, "%")
    }
    fit(wkf_template, training(fold))
  }
  
  v_wkfs <- mec_folds$splits |>
    imap(progressive_fit)
  if (verbose) {
    cat("\r", "100 %")
    print("Finished training!")
  }
  
  # Creating testing analysis dataset
  wkf_augs <- pmap(list(v_wkfs, mec_folds$splits, 1:length(v_wkfs)),
                   function(wkf, split, id) augment(wkf, testing(split)) |>
                     mutate(wkf_id = id)) |>
    bind_rows()
  
  performance_summary <- get_performance_summary(config, wkf_augs)
  
  save_path <- v_path(config$version)
  if (config$model$model$name == "MLP Neural Network") {
    v_wkfs <- map(v_wkfs, bundle)
  }
  
  v_wkfs |>
    saveRDS(file.path(save_path, "model", "model_fits.csv.gz"))
  wkf_augs |>
    readr::write_csv(file.path(save_path, "model", "testing_results.csv.gz"))
  performance_summary |>
    yaml::write_yaml(file.path(save_path, "model", "performance_summary.yaml"))
  
  v_wkfs
}
