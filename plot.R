source("setup.R")

### Plotting function
fitted_wkf <- readRDS("test_model_v0.csv.gz")

root_md <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/model_data"
explotdata <- readr::read_csv(file.path(root_md, "2015_07_01_exdata_ET.csv.gz"))

explotdata_predictable <- explotdata |>
  
coper_predictable <- coper_data |>
  as_tibble() |>
  na.omit() |>
  mutate(vel = sqrt(uo^2 + vo^2), 
         month = lubridate::month(date) |> as.factor(levels = 1:12))

coper_preds <- predict(fitted_wkf, coper_predictable)
