species <- "cfin" # arbitrary
source("setup.R")
library(terra)

coper_path <- copernicus_path(file.path("chfc", "GLOBAL_MULTIYEAR_PHY_001_030"))

copernicus_bathymetry <- andreas::read_static("deptho", coper_path)

## Setting NA values to zero for calculation
na_indices <- which(is.na(copernicus_bathymetry[[1]]))
copernicus_bathymetry[[1]][na_indices] <- 0

## Convert to SpatRaster and use terra::terrain() function to calculate slope
cop_sr <- as(copernicus_bathymetry, "SpatRaster")
bathy_slope_sr <- terra::terrain(cop_sr, v = "slope", neighbors = 8, unit = "degrees")
bathy_slope <- st_as_stars(bathy_slope_sr)

p <- ggplot() + 
  geom_stars(data = bathy_slope) + 
  scale_fill_viridis(option = "turbo") + 
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude", title = "Bathymetric slope")

p

### Saving material to file
root_gendata <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/_general_data"
write_stars(bathy_slope, layer = 1, 
            dsn = file.path(root_gendata, "bathymetric_slope_terra.tif"))

pdf(file.path(root_gendata, "plots", "bathymetric_slope_terra.pdf"))
print(p)
dev.off()

