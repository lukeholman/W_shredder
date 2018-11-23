setwd("/data/projects/punim0243/W_shredder")

#############################################
# Load all custom functions and packages
#############################################
source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")
custom_functions <- ls()

#############################################
# Define the entire parameter space to be run
#############################################
parameters <- expand.grid(
  release_size = 20,
  release_strategy = c("one_patch", "all_patches"),
  W_shredding_rate = c(0.50, 0.95, 1), # strength of gene drive in females
  Z_conversion_rate = c(0, 0.5, 0.95), # strength of gene drive in males
  Zr_creation_rate = c(0, 0.001, 0.01, 0.1), # frequency of NHEJ in males
  Zr_mutation_rate = c(0.0, 0.00001),
  Wr_mutation_rate = c(0.0, 0.00001),
  cost_Zdrive_female = c(0.01, 0.1, 0.5, 1), # Cost of Z* to female fecundity
  cost_Zdrive_male = c(0.01, 0.2),  # Cost of Z* to male mating success
  male_migration_prob = c(0.05, 0.5),
  female_migration_prob = c(0.05, 0.5),
  migration_type = c("local", "global"),
  n_patches = c(2, 20),
  softness = c(0, 0.5, 1),
  male_weighting = c(0.5, 1, 1.5),
  density_dependence_shape = c(0.2, 1, 1.8),
  cost_Wr = 0,   # Assume resistance is not costly for now. Seems pretty obvious how this affects evolution
  cost_Zr = 0,
  cost_A = 0,
  cost_B = 0,
  max_fecundity = c(50, 100),
  carrying_capacity = 10000,
  initial_pop_size = 10000,
  initial_Zdrive = 0,
  initial_Zr = 0.00,
  initial_Wr = 0.00,
  initial_A = 0.00,
  initial_B = 0.00,
  realisations = 1, # change to e.g. 1:100 for replication
  generations = 1000,
  burn_in = 50
) %>% filter(!(W_shredding_rate == 0 & Z_conversion_rate == 0)) %>%
  mutate(migration_type = as.character(migration_type),
         release_strategy = as.character(release_strategy))

# Shuffle for even workload across all cores
set.seed(1)
parameters <- parameters[sample(nrow(parameters)), ]

# Set the initial frequency to be the same as the mutation rate for the resistant chromosomes
parameters$initial_Wr <- parameters$Wr_mutation_rate
parameters$initial_Zr <- parameters$Zr_mutation_rate

# No point doing lots of W_shredding_rate when cost_Zdrive_female == 1
parameters$W_shredding_rate[parameters$cost_Zdrive_female == 1] <- 1
parameters <- parameters %>% distinct()
num_parameter_spaces <- nrow(parameters)

#############################################################################
# Create a data frame of parameter spaces that have been completed already
#############################################################################

# Dataframe of parameter spaces that are finished
done <- list.files("data/sim_results", full.names = TRUE)

# Dataframe of parameter spaces that are finished
if(length(done) != 0){
  finished <- mclapply(done, function(x) read_rds(x)[[1]], mc.cores = 8) %>%
    bind_rows() %>%
    select(-generation_extinct, -generation_Zd_extinct,
           -generation_W_extinct, -generation_Zd_fixed, -outcome)
  # Check all the column names are the same! Should be, if all parameters were made using same code
  if(!identical(names(parameters), names(finished))) print("Error! Delete results and start afresh")
}

#############################################################################
# If not overwriting, remove rows from `parameters` that are already finished
#############################################################################
over_write <- FALSE; finished <- numeric(0)

if(!over_write && length(done) != 0){
  finished <- apply(finished, 1, paste0, collapse = "_")
  to_do    <- as.character(apply(parameters, 1, paste0, collapse = "_"))
  to_do <- str_replace_all(to_do, " ", "")
  parameters <- parameters[!(to_do %in% finished), ]
}

saveRDS(parameters, "parameters_left_to_do.rds")
