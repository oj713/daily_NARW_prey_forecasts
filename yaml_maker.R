v_num <- "0.01"
overwrite <- TRUE
species_class <- c("jellyfish", "right whale prey")[[2]] # What's the purpose of this data?
species <- c("coelenterates", "pseudocalanus", "centropages", 
             "combjellies", "siphonophora", "salpa", "cfin")[[3]]
v_name <- paste0(substr(species, 1, 4), ".", v_num)

note <- "Centropages model. Revised dataset, 80% quantile threshold."

source("setup.R")

# training data 
strata = "patch" # currently unread
coper_data_config <- list(vars_static = c("bathy_depth"), #, "bathy_slope"),
                          vars_phys = c("bottomT", "mlotst", "so", "thetao", "vel", "zos"),
                          vars_bgc = NULL, #c("chl", "no3", "nppv", "o2", "po4", "si"),
                          vars_time = c("day_length", "ddx_day_length"))
species_data_config <- list(ecomon_column = c("coel_m2", "pseudo_m2", "ctyp_m2",
                                              "ctenop_m2", "siph_m2", "salps_m2")[[2]],
                            # Prefix for saved data in input_data
                            alt_source = NULL, #"corrected_CIV_CVI_m2" for cfin
                            threshold = list(pre = c(0, 7500, 24000)[[3]],
                                             post = NULL)) # currently unread
# model
seed <- 750

brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 500,
            learn_rate = .1,
            tree_depth = 4,
            mtry = 5,
            min_n = 10, 
            nthread = 4)

transformations <- c("step_log_bathy", "step_normalize_numeric")

### ASSEMBLY #############################################

training_data_config <- list(species = species, 
                             species_data = species_data_config, 
                             coper_data = coper_data_config)

model_config <- list(seed = seed,
                     model = brt, 
                     transformations = transformations)

config <- list(version = v_name, 
               class = species_class,
               note = note,
               training_data = training_data_config, 
               model = model_config)

### TYPE CHECKS ###########################################
if (!is.null(species_data_config$ecomon_column) & !is.null(species_data_config$alt_source)) {
  stop ("One of ecomon_column or alt_source must be blank for yaml configuration.")
}
if (model_config$model$name != "Boosted Regression Tree") {
  stop("Model types other than BRT are currently not supported.")
}

### WRITING TO FILE
write_config(config, overwrite = overwrite)

