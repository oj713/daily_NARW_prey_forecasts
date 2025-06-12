library(viridis)
library(ggplot2)
library(copernicus)
library(andreas)

### Plotting function
fitted_wkf <- readRDS("test_model_v0.csv.gz")

coper_predictable <- coper_data |>
  as_tibble() |>
  na.omit() |>
  mutate(vel = sqrt(uo^2 + vo^2), 
         month = lubridate::month(date) |> as.factor(levels = 1:12))

coper_preds <- predict(fitted_wkf, coper_predictable)
