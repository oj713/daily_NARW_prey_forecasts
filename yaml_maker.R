v_num <- "0.00"
overwrite <- TRUE
species <- c("jellyfish", "cfin", "pseudocalanus", "centrophages")[[2]]
v_name <- paste0(substr(species, 1, 2), ".", v_num)

note <- "Initial cfin model. Indifferent to tuning, day length time representation, all physical variables"

source("setup.R")

# training data 
strata = "patch" # currently unread
coper_data_config <- list(vars_static = "bathy_depth", 
                          vars_phys = c("bottomT", "mlotst", "so", "thetao", "vel", "zos"),
                          vars_bgc = NULL, #c("chl", "no3", "nppv", "o2", "po4", "si"),
                          vars_time = c("day_length", "ddx_day_length"))
species_data_config <- list(source = "ecomon", # currently unread
                            data_columns = c("corrected_CIV_CVI_m2"), # currently unread
                            vertically_corrected = TRUE, # currently unread
                            threshold = list(pre = 30000,
                                             post = NULL)) # currently unread
# model
seed <- 799

brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 500,
            learn_rate = .1,
            tree_depth = 4,
            mtry = 5,
            min_n = 10)

transformations <- c("step_log_bathy", "step_normalize_numeric")

### ASSEMBLY #############################################

training_data_config <- list(species = species, 
                             species_data = species_data_config, 
                             coper_data = coper_data_config)

model_config <- list(seed = seed,
                     model = brt, 
                     transformations = transformations)

config <- list(version = v_name, 
               note = note,
               training_data = training_data_config, 
               model = model_config)

### WRITING TO FILE
write_config(config, overwrite = overwrite)

