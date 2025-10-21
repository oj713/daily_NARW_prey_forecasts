v_num <- "0.00"
overwrite <- TRUE 
# What's the purpose of this data?
species_class <- c("jellyfish", "right whale prey")[[1]] 
species <- c("coelenterates", "pseudocalanus", "centropages", 
             "combjellies", "siphonophora", "salpa", "cfin")[[8]]
# Version names are ALWAYS the first four letters of the species + version number
v_name <- paste0(substr(species, 1, 4), ".", v_num)

# Elaboration on the significance of this model.
note <- "Note goes here."

source("setup.R") # setup.R should usually be sourced AFTER defining species. 

# training data 
strata = "patch" # Unread, means to say that training data is stratified by patch
# What copernicus variables are being used for this model? bottomT = tob
coper_data_config <- list(vars_static = c("bathy_depth"), #, "bathy_slope"),
                          vars_phys = c("bottomT", "mlotst", "so", "thetao", "vel", "zos"),
                          vars_bgc = c("chl", "no3", "nppv", "o2", "po4", "si"), #NULL
                          vars_time = c("day_length", "ddx_day_length"))
# What species data are we using?
species_data_config <- list(ecomon_column = c("coel_m2", "pseudo_m2", "ctyp_m2",
                                              "ctenop_m2", "siph_m2", "salps_m2")[[7]], #or NULL
                            # alt_source is an alterative file to be read. must be file name (sans .csv.gz) of valid file in species/input_data folder
                            # ecomon_column should be NULL if alt_source isn't. 
                            alt_source = NULL, #"corrected_CIV_CVI_m2" for cfin
                            threshold = list(pre = c(0, 7500, 24000)[[1]], # presence threshold
                                             post = NULL)) # unread
# model
seed <- 750

# in theory "MLP Neural Network" with engine "keras" would also be supported here but I haven't tested it. 
brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 500,
            learn_rate = .1,
            tree_depth = 4,
            mtry = 5,
            min_n = 10, 
            nthread = 4)

# corresponds to named functions in the build_workflows.R file. Steps to apply to recipe. 
# If you would like to build your own it must accept only recipe object and return a recipe object.
transformations <- c("step_log_bathy", "step_normalize_numeric")

### ASSEMBLY, DO NOT EDIT #############################################

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

