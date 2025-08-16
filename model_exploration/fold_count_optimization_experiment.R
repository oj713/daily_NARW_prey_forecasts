species <- "cfin"
source("setup.R")
source("data_preparation/data_from_config.R")
source("build_workflows.R")

## create_version.R
v <- "cf.0.00"
config <- read_config(v)
set.seed(config$model$seed)
mec <- data_from_config(config)
dplyr::count(mec, patch) |> mutate(prop = n/sum(n))

K = 100
mec_folds <- mec |>
  mc_cv(prop = .75, times = K, strata = patch)

## build_workflows.R // init_v_wkf()
n_folds <- nrow(mec_folds)
template_data <- mec_folds$splits[[1]] |> training()
wkf_template <- workflow_from_config(config, template_data)

progressive_fit <- function(fold, idx) {
  cat("\r", round((idx - 1)/n_folds, 2) * 100, "%")
  fit(wkf_template, training(fold))
}

v_wkfs <- mec_folds$splits |>
  imap(progressive_fit)

# Creating testing analysis dataset
wkf_augs <- pmap(list(v_wkfs, mec_folds$splits, 1:length(v_wkfs)),
                 function(wkf, split, id) augment(wkf, testing(split)) |>
                   mutate(wkf_id = id)) |>
  bind_rows()

##### Comparison across fold counts

#' Retrieves a dataframe of performance for fold number
fold_count_performance <- function(fold_num) {
  fold_num_augs <- wkf_augs |> 
    filter(wkf_id <= fold_num)
  summary <- get_performance_summary(config, fold_num_augs)
  do.call(cbind, summary$performance_statistics) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column("metric") |>
    mutate(fold_num = fold_num)
}

fold_count_res <- 5:100 |>
  lapply(fold_count_performance) |>
  bind_rows() |>
  mutate(across(c(`5%`, `50%`, `95%`), as.numeric))

ggplot(fold_count_res, aes(x = fold_num, col = metric, fill = metric)) + 
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = .2, col = NA) +
  geom_line(aes(y = `50%`)) + 
  facet_wrap(~metric, scale = "free") +
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Number of Model Folds", y = "Median and 95% Range of Metric",
       title = "Performance vs. Model Fold Count, cf.0.00")

