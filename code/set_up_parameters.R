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
print("Defining parameter space")

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
  initial_A = c(0, 0.05),
  initial_B = c(0, 0.05),
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

# No point doing lots of different W_shredding_rate values when cost_Zdrive_female == 1
parameters$W_shredding_rate[parameters$cost_Zdrive_female == 1] <- 1
parameters <- parameters %>% distinct()
num_parameter_spaces <- nrow(parameters)

#############################################################################
# Create a data frame of parameter spaces that have been completed already
# and remove rows from `parameters` that are already finished
#############################################################################

print("Checking previously-completed files...")

completed <- readRDS("data/all_results.rds") %>%
  select(!! names(parameters))
completed <- apply(completed, 1, paste0, collapse = "_")

to_do <- data.frame(row = 1:nrow(parameters),
                    pasted = apply(parameters, 1, paste0, collapse = "_"),
                    stringsAsFactors = FALSE) %>%
  filter(!(pasted %in% completed))

parameters <- parameters[to_do$row, ]
print(paste("Already completed", length(completed), "parameter spaces"))
print(paste("Queing up", nrow(parameters), "model runs"))
rm(to_do)









