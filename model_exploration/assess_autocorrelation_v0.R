# Run model_test_v0.R first

my_metrics(results, truth = patch, estimate = .pred_class, .pred_TRUE, event_level = "second")

results <- mutate(results, pred_resid = .pred_TRUE - (as.numeric(patch)) + 1)


library(spdep)

# Assuming 'results' has lon and lat columns
coords <- results |> select(longitude, latitude)
# duplicated rows?
dup_coords <- coords[duplicated(coords), ]
nrow(dup_coords)
coords_jittered <- coords |>
  mutate(across(everything(), ~ jitter(.x, amount = 1e-5)))

nb <- knn2nb(knearneigh(coords_jittered, k = 5))
lw <- nb2listw(nb, style = "W")

moran.test(results$pred_resid, lw)
