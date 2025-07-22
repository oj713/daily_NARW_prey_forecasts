source("setup.R")

jec <- get_input_data("jelly_phys_bgc_bathy_ET_chfc.csv.gz")

abundance_threshold <- 0

jec <- jec |>
  mutate(presence = (coel_m2 > abundance_threshold) |> factor(), 
         vel = sqrt(uo^2 + vo^2),
         month = lubridate::month(date) |> factor(levels = 1:12))

jec_split <- initial_split(jec, prop = .75, strata = presence)

### Create a workflow
recipe_spec <- 
  recipe(presence ~ bathy_depth + vel + month + mlotst + bottomT + thetao + so +
           zos + chl + no3 + nppv + o2 + po4 + si,
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

workflow_template <- workflow() |>
  add_recipe(recipe_spec) |>
  add_model(model_spec)

fitted_wkf <- fit(workflow_template, training(jec_split))

## Evaluate the model

results <- augment(fitted_wkf, testing(jec_split))

my_metrics <- metric_set(roc_auc, sens, spec, accuracy)

my_metrics(results, truth = presence, estimate = .pred_class, .pred_TRUE, event_level = "second")

fitted_wkf |>
  saveRDS("model_exploration/jelly_test_model_v0.csv.gz")


var_imp <- function() {
  vimp <- extract_fit_parsnip(fitted_wkf) |>
    vi_model()
  
  month <- vimp |>
    filter(startsWith(Variable, "month")) |>
    summarize(Variable = 'month', 
              Importance = sum(Importance), .groups = 'keep')
  
  agg_vimp <- vimp |>
    filter(!startsWith(Variable, "month"))
  renamed_vimp <- bind_rows(agg_vimp, month) |>
    arrange(Importance) |>
    #mutate(across(Variable, ~var_abb()[Variable] |> unlist())) |>
    mutate(across(Variable, ~factor(.x, levels = Variable)))
  
  if (plot) {
    p <- ggplot(renamed_vimp, aes(x = Importance, y = Variable)) +
      theme(axis.title.y = element_blank()) +
      theme_bw() +
      theme(axis.title.y = element_blank()) +
      geom_bar(stat = 'identity', fill = "dodgerblue4")
    
    save_analysis(p, v, "var_importance")
  }
  
  bind_rows(agg_vimp, month) |>
    arrange(desc(Importance))
}
