species <- ""
source("setup.R")
library(ecomon)

# Variability in the nutritional value of the major copepods in Cape Cod Bay (Massachusetts, USA) 
# with implications for right whales
# https://doi.org/10.1111/j.1439-0485.2006.00087.x
# 10.3354/meps07832

species_to_choose <- list(
  list("name" = "Pseudocalanus", "ecomon_column" = "pseudo_m2"),
  list("name" = "Centropages", "ecomon_column"  = "ctyp_m2")
)
speclist <- c(sapply(species_to_choose, function(x) x$ecomon_column), "calfin_m2")

edata <- readr::read_csv(get_path_main("_general_data", "ecomon_copernicus_matched.csv.gz"),
                             col_types = readr::cols()) |>
  select(lat, lon, date, all_of(speclist))

e_longer <- edata |>
  pivot_longer(all_of(speclist))

ggplot(e_longer, aes(x = log(value + 1))) + 
  geom_histogram(bins = 30, fill = "blue") + 
  facet_wrap(~name)

rough_percentage <- ecdf(edata$calfin_m2)(30000)

quantile(edata$pseudo_m2, rough_percentage, na.rm = TRUE)
quantile(edata$ctyp_m2, rough_percentage, na.rm = TRUE)

quantile(edata$pseudo_m2, .95, na.rm = TRUE)
quantile(edata$ctyp_m2, .95, na.rm = TRUE)

###### Conduct formal analysis
generate_performance_figure <- function(species, v) {
  root <<- get_root(species)
  vdata <- data_from_config(read_config(v))
  vdata <- vdata |> select(-patch)
  vdata_folds <- vdata |>
    vfold_cv(v = 10, repeats = 1, strata = ind_m2)
  
  recipe_from_threshold <- function(threshold) {
    recipe(vdata, ~ bathy_depth + bottomT + mlotst + so + thetao + vel + zos + day_length + ddx_day_length + ind_m2) |>
      update_role(ind_m2, new_role = "ID") |>
      step_log(bathy_depth, offset = 1) |>
      step_normalize(all_numeric_predictors()) |>
      step_mutate(patch = (ind_m2 > threshold) |> as.numeric() |> factor(levels = c("1", "0")),
                  role = "outcome", skip = FALSE, id = paste("threshold", threshold))
  }
  quants <- quantile(vdata$ind_m2, probs = c(.8, .85, .9, .95))
  pseudorecipes <- quants |>
    map(recipe_from_threshold)
  
  modeltemp <- boost_tree() |>
    set_mode("classification") |>
    set_engine("xgboost") |>
    set_args(trees = 500,
             learn_rate = .1,
             tree_depth = 4,
             mtry = 5,
             min_n = 10, 
             nthread = 4)
  
  thresholdwkfs <- workflow_set(pseudorecipes, list(modeltemp), cross = TRUE)
  
  fitted_wkfs <- thresholdwkfs |>
    workflow_map(fn = "fit_resamples",
                 verbose = FALSE, 
                 seed = 400, 
                 resamples = vdata_folds, 
                 metrics =  metric_set(roc_auc, f_meas, accuracy, sens, spec),
                 control = control_resamples(save_pred = TRUE, save_workflow = TRUE))
  
  #' Retrieves a dataframe of performance for fold number
  ranked <- rank_results(fitted_wkfs) |>
    mutate(threshold = gsub("_boost_tree", "", wflow_id)) |>
    select(threshold, .metric, mean, std_err)
  
  p <- ggplot(ranked, aes(x = threshold)) +
    geom_point(aes(y = mean)) + 
    facet_wrap(~.metric) +
    geom_errorbar(aes(ymin = mean - 2*std_err, ymax = mean + 2*std_err),
                  alpha = .3, width = .3, col = "blue") + 
    theme_bw() + 
    labs(title = paste0(str_to_title(species), ": Percentile patch threshold vs. performance"),
         x = paste0("Quantile patch threshold (= ", paste(round(quants), collapse = ", "), " ind/m2)"), 
         y = "Mean performance (num. folds = 10)")
  
  pdf(v_path(v, "model", "patchthreshold_vs_performance.pdf"))
  print(p)
  dev.off()
  
  list("Quants" = quants, "Results" = fitted_wkfs, "Plot" = p) 
}

pseudo_res <- generate_performance_figure("pseudocalanus", "pseu.0.00")

centrop_res <- generate_performance_figure("centropages", "cent.0.00")
