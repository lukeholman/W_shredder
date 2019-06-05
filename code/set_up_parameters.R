#######################################################
# Load all custom functions and packages
#######################################################
source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")


#######################################################
# Define possible parameter ranges for latin hypercube
#######################################################
parameter_ranges <- data.frame(
  release_size = c(10, 100),
  release_strategy = c(0, 1),   # binary variable: local or global release
  W_shredding_rate = c(0.4, 1), # p-shred in the paper
  Z_conversion_rate = c(0, 1), # p-conv in the paper
  Zr_creation_rate = c(0, 0.1), # p-nhej in the paper
  Zr_mutation_rate = c(0.0, 0.00001), # mu-Z
  Wr_mutation_rate = c(0.0, 0.00001), # mu-W
  cost_Zdrive_female = c(0, 0.6),     # Cost of Z* to female fecundity
  cost_Zdrive_male   = c(0, 0.6),     # Cost of Z* to male mating success
  male_migration_prob = c(0.001, 0.5),
  female_migration_prob = c(0.001, 0.5),
  migration_type = c(0, 1), # binary variable: do migrants move to next door patch, or a random patch anywhere in the world?
  n_patches = c(2, 50), # integer number of patches
  max_fecundity = c(10, 1000), # r in the paper
  softness = c(0, 1), # psi in the paper
  male_weighting = c(0.1, 1.9), # delta in the paper
  density_dependence_shape = c(0.1, 1.9), # alpha in the paper
  initial_A = c(0, 1),
  initial_B = c(0, 1)
)

#######################################################
# Draw n samples from the latin hypercube
#######################################################
make_latin_hyper_cube_parameter_space <- function(parameter_ranges, n_samples){
  n_parameters <- ncol(parameter_ranges)
  X <- randomLHS(n_samples, n_parameters)

  for(i in 1:n_parameters){
    X[,i] <- parameter_ranges[1, i] + (parameter_ranges[2, i] - parameter_ranges[1, i]) * X[, i]
  }

  colnames(X) <- colnames(parameter_ranges)
  as.data.frame(X) %>%
    mutate(n_patches = round(n_patches),
           release_size = round(release_size),
           release_strategy = ifelse(release_strategy < 0.5, "one_patch", "all_patches"),
           migration_type = ifelse(migration_type < 0.5, "local", "global"),
           initial_A = ifelse(initial_A < 0.5, 0, 0.05),
           initial_B = ifelse(initial_B < 0.5, 0, 0.05),
           cost_Wr = 0,
           cost_Zr = 0,
           cost_A = 0,
           cost_B = 0,
           carrying_capacity = 10000,
           initial_pop_size = 10000,
           initial_Zdrive = 0,
           initial_Zr = 0.00,
           initial_Wr = 0.00,
           realisations = 1, # change to e.g. 1:100 for replication
           generations = 1000,
           burn_in = 50
    )
}

n_parameter_spaces <- 10^6
print(paste("Sampling", n_parameter_spaces, "parameter spaces from a Latin hypercube..."))
parameters <- make_latin_hyper_cube_parameter_space(parameter_ranges, n_parameter_spaces)
print("...done sampling.")
