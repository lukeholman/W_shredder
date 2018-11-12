## ----setup, echo=FALSE, results='hide'-----------------------------------
source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")
custom_functions <- ls()

## ------------------------------------------------------------------------
set.seed(1)
parameters <- expand.grid(
  release_size = 20,
  release_strategy = c("one_patch", "all_patches"),
  W_shredding_rate = c(0.95, 1),
  Z_conversion_rate = c(0, 0.5, 0.95),
  Zr_creation_rate = c(0, 0.001, 0.01),
  Zr_mutation_rate = c(0.0, 0.00001),
  Wr_mutation_rate = c(0.0, 0.00001),
  cost_Zdrive_female = c(0.01, 0.1, 0.5),
  male_migration_prob = c(0.05, 0.5),
  female_migration_prob = c(0.05, 0.5),
  migration_type = c("local", "global"),
  softness = c(0, 0.5, 1),
  male_weighting = c(0.8, 1, 1.2),
  density_dependence_shape = 0.2,
  cost_Zdrive_male = c(0.01, 0.1),
  cost_Wr = 0.05,
  cost_Zr = 0.05,
  cost_A = 0.01,
  cost_B = 0.01,
  n_patches = c(2, 20),
  max_fecundity = 50,
  carrying_capacity = 10000,
  initial_pop_size = 10000,
  initial_Zdrive = 0,
  initial_Zr = 0.00,
  initial_Wr = 0.00,
  initial_A = 0.00,
  initial_B = 0.00,
  realisations = 1, # change to e.g. 1:100 for replication
  generations = 900,
  burn_in = 50
) %>% filter(!(W_shredding_rate == 0 & Z_conversion_rate == 0))
parameters <- parameters[sample(nrow(parameters)), ]
# Set the initial frequency to teh mutation rate for the resistant chromosomes
parameters$initial_Wr <- parameters$Wr_mutation_rate
parameters$initial_Zr <- parameters$Zr_mutation_rate

## ------------------------------------------------------------------------
keep_going <- TRUE
while(keep_going) do_all_parameters(parameters, over_write = FALSE, cores = 8)
combine_results_files(cores = 8) %>% saveRDS(file = "data/all_results.rds") 

## ------------------------------------------------------------------------
library(rslurm)
# module load R/3.4.0-GCC-4.9.2
# chunked_parameters <- split(parameters, 
#                             (as.numeric(rownames(parameters))-1) %/% 10000)

sopt <- list(time = '12:00:00') # time in hours

sjob <- slurm_apply(function(i) do_all_parameters(parameters[[i]], 
                                                  over_write = FALSE, 
                                                  cores = 8,
                                                  on_cluster = TRUE), 
                    data.frame(i = 1:nrow(parameters)),
                    add_objects = c("do_all_parameters", 
                                    "parameters",
                                    custom_functions),
                    jobname = 'W_shredder',
                    nodes = 30, cpus_per_node = 8, slurm_options = sopt)

