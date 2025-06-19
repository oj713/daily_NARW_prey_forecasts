source("setup.R")

### Plotting function
fitted_wkf <- readRDS("model_exploration/test_model_v0.csv.gz")
explotdata <- get_input_data("2015_07_01_exdata_ET.csv.gz")

explotdata_predictable <- explotdata |>
  mutate(vel = sqrt(uo^2 + vo^2), month = 7 |> factor(levels = 1:12))

coper_preds <- augment(fitted_wkf, explotdata_predictable, type = "prob")

ggplot(coper_preds, aes(x = x, y = y, fill = .pred_TRUE)) + 
  geom_raster() + 
  coord_quickmap() + 
  scale_fill_viridis(option = "turbo")
