species <- "cfin"
v <- "cfin.0.01"
source("setup.R")
source("io_stars.R")

##### Brickman helper
read_brickman_v6_01_02 <- function() {
  
  folder <- file.path("/mnt/ecocast/projectdata/students/ojohnson/brickman/versions",
                      "v6", "v6.01.02", "pred", "PRESENT")
  
  wd <- getwd()
  setwd(folder)
  
  # helper method that retrieves data for a month
  read_month <- function(mon) {
    filename <- paste0("quant_preds", mon, ".csv.gz")
    
    if (file.exists(filename)) {
      readr::read_csv(filename, col_types = readr::cols())
    } else {
      NULL
    }
  }
  
  # constructing list of prediction data named by month
  preds <- setNames(1:12, month.abb) |>
    lapply(read_month)
  
  setwd(wd)
  preds
}

brickmanpreds <- read_brickman_v6_01_02()

coperpreds <- read_quantile_stars(v_pred_path(v, "monthly"))
coperpreds <- aggregate(coperpreds["50%",,,], by = function(d) (lubridate::month(d)), FUN = mean)

# Converting brickmanpreds to stars

tibblebrickman <- dplyr::bind_rows(brickmanpreds) |>
  select(brickman = `50%`, lat, lon, month)

tibblecoper <- as_tibble(coperpreds) |> na.omit() |>
  select(coper = `50%`, lon, lat, month = geometry) |>
  mutate(month = as.numeric(month))

rm(brickmanpreds, coperpreds)
gc()

coperaligned <- mutate(tibblecoper, across(c(lon, lat), ~round(.x * 4)/4)) |>
  group_by(lat, lon, month) |>
  summarise(coper = mean(coper), .groups = "keep")
brickmanaligned <- mutate(tibblebrickman, across(c(lon, lat), ~round(.x * 4)/4)) |>
  group_by(lat, lon, month) |>
  summarise(brickman = mean(brickman), .groups = "keep")

joinedres <- left_join(coperaligned, brickmanaligned) |>
  na.omit() |>
  mutate(diff = coper - brickman)

xlim <- range(joinedres$lon)
ylim <- range(joinedres$lat)

p1 <- ggplot(joinedres, aes(x = lon, y = lat, col = brickman)) +
  facet_wrap(~month) + 
  geom_point(cex = .2) + 
  scale_color_viridis() + 
  coord_quickmap()

p2 <- ggplot(joinedres, aes(x = lon, y = lat, col = coper)) +
  facet_wrap(~month) + 
  geom_point(cex = .2) + 
  scale_color_viridis() + 
  coord_quickmap()

p <- ggplot(joinedres, aes(x = lon, y = lat, col = diff)) + 
  facet_wrap(~month) + 
  geom_point(cex = .2) + 
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group),
               fill = "black", col = "black") +
  scale_color_gradientn(limits = c(-1, 1),
                        colors = c("darkorchid4",
                                   "darkslateblue",
                                   "royalblue3", 
                                   "deepskyblue2", 
                                   "white", 
                                   "goldenrod2",
                                   "darkorange1",
                                   "orangered3", 
                                   "red4"),
                        na.value = "white") +
  coord_quickmap(xlim = xlim, ylim = ylim) +
  ggtitle("Copernicus minus Brickman Cfin patch probability")

pdf(v_path(v, "preds", "daily_resolution", "copernicus_vs_brickman_comparison.pdf"))
print(list(p, p1, p2))
dev.off()


