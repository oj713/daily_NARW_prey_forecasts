#### GENERATES HISTORICAL AVERAGES FOR SPECIES OF INTEREST
### RUN ONE TIME FOR NEW SPECIES PREVIOUSLY WITHOUT THIS FILE
species <- ""
source("setup.R")
source("io_stars.R")

regions_sf <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)

shiny_species <- list(
  # list("spec" = "coelenterates", "v" = "coel.1.00"), 
  # list("spec" = "salpa", "v" = "salp.1.00"),
  # list("spec" = "siphonophora", "v" = "siph.1.00"),
  list("spec" = "cfin", "v" = "cfin.0.01"),
  list("spec" = "pseudocalanus", "v" = "pseu.0.01"),
  list("spec" = "centropages", "v" = "cent.0.01")
)

# Iterate through each species
for (shiny_spec in shiny_species) {
  gc()
  
  # Meta information
  species <- shiny_spec$spec
  v <- shiny_spec$v
  root <- get_root(species)
  
  # Initializing directory
  monthly_root <- file.path(root, "shiny_data", "monthly_aggregates")


  if (dir.exists(monthly_root)) { next } else { dir.create(monthly_root, recursive = TRUE) }
  cat(species, "...")

  # Creating the monthly aggregations object
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  ym_stars <- ym_stars[regions_sf]
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,],
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$uncertainty <- m_agg$`95%` - m_agg$`5%`

  #' Helper: Extracts a single month from the aggregation object and saves to file
  save_month_qs <- function(m) {
    save_file <- file.path(monthly_root, paste0(species, "_monthly_aggregate_m", m, ".nc"))

    write_quantile_stars(m_agg[,m,,,drop = TRUE], save_file, as_float = TRUE)

    TRUE
  }

  success <- 1:12 |> lapply(save_month_qs)
}


