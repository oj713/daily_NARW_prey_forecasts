source("setup.R")

### Plotting function
fitted_wkf <- readRDS("model_exploration/test_model_v0.csv.gz")
explotdata <- get_input_data("2015_07_01_exdata_ET.csv.gz")

explotdata_predictable <- explotdata |>
  mutate(vel = sqrt(uo^2 + vo^2), month = 7 |> factor(levels = 1:12))

coper_preds <- augment(fitted_wkf, explotdata_predictable, type = "prob")

posfunc <- stephane_final(state_val = "rest", post = TRUE)

coper_preds <- mutate(coper_preds, new_val = .pred_TRUE * posfunc(bathy_depth))

p <- ggplot(coper_preds, aes(x = x, y = y, fill = .pred_TRUE)) + 
  geom_raster() + 
  coord_quickmap() + 
  scale_fill_viridis(option = "turbo") +
  labs(title = "Cfin aggregation probability, 2015-07-01", fill = "Probability", 
       y = "Latitude", x = "Longitude") + 
  theme_bw() + 
  theme(legend.position = "bottom")

save_pdf_ecocast(p, "plot_raw_2015_07_01", root = "model_exploration")



