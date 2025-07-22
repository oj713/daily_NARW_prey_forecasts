v_num <- "0.00"
overwrite <- FALSE
species <- c("jellyfish", "cfin", "pseudocalanus", "centrophages")[[1]]
v_name <- paste0(substr(species, 1, 2), ".", v_num)

note <- "Initial jellyfish model. Indifferent to tuning, day length time representation, all base variables."

source("setup.R")

# training data 
strata = "patch"
coper_data_config <- list(vars_static = "bathy_depth", 
                          vars_phys = c("bottomT", "mlotst", "so", "thetao", "uo", "vo", "zos"),
                          vars_bgc = c("chl", "no3", "nppv", "o2", "po4", "si"),
                          vars_time = c("day_length", "ddx_day_length"),
                          transform = c("vel = sqrt(uo^2 + vo^2)",
                                        "bathy_depth = log10(bathy_depth + 1)",
                                        "step_normalize()"))
species_data_config <- list(source = "ecomon", 
                            data_columns = c("coel_m2"),
                            vertically_corrected = FALSE, 
                            threshold = list(pre = 0,
                                             post = NULL))
# model
seed <- 799

brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 500,
            learn_rate = .1,
            tree_depth = 4,
            mtry = 5,
            min_n = 10)

model_types <- list(brt) 

### ASSEMBLY #############################################

training_data_config <- list(species = species, 
                             species_data = species_data_config, 
                             coper_data = coper_data_config)

model_config <- list(seed = seed,
                     model_list = model_types)

config <- list(version = v_name, 
               note = note,
               training_data = training_data_config, 
               model = model_config)

### WRITING TO FILE
write_config(config, overwrite = overwrite)

