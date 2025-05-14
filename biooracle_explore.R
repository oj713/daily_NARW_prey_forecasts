library(biooracle) # from Ben
library(biooracler) # From Bio Oracle team
library(stars)

# Test bounding box
nwa_bb <- c(xmin = -77, xmax = -42.5, ymin = 36.5, ymax = 56.7)

set_biooracle_root("/mnt/ecocast/projectdata/students/ojohnson/biooracle")

nwa_path = biooracle_path("nwa") |> make_path()
biooracle_path() |> dir(full.names = TRUE)

list_layers()

# https://erddap.bio-oracle.org/erddap/griddap
dataset_id = "thetao_ssp119_2020_2100_depthmin"
newfile = fetch_biooracle(dataset_id, 
                          bb = c(xmin = -77, xmax = -42.5, ymin = 36.5, ymax = 56.7))

x <- stars::read_stars(newfile, quiet = TRUE)
print(x)
