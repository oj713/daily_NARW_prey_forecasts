species <- ""
source("setup.R")
library(ecomon)

# Variability in the nutritional value of the major copepods in Cape Cod Bay (Massachusetts, USA) 
# with implications for right whales
# https://doi.org/10.1111/j.1439-0485.2006.00087.x
# 10.3354/meps07832

species_to_choose <- list(
  list("name" = "Pseudocalanus", "ecomon_column" = "pseudo_m2"),
  list("name" = "Centropages", "ecomon_column"  = "ctyp_m2")
)
speclist <- c(sapply(species_to_choose, function(x) x$ecomon_column), "calfin_m2")

edata <- ecomon::scale_ecomon() |>
  select(lat, lon, date, all_of(speclist))

e_longer <- edata |>
  pivot_longer(all_of(speclist))

ggplot(e_longer, aes(x = log(value + 1))) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~name)

rough_percentage <- ecdf(edata$calfin_m2)(30000)

quantile(edata$pseudo_m2, rough_percentage, na.rm = TRUE)
