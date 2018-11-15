source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")

cpus <- 8
sopt <- list(time = '2:00:00',   # time in hours
             mem  = '32768')     # 32GB memory across all 8 cores


sjob <- slurm_apply(function(i) {
  saveRDS(combine_results_files(cores = 8),
          file = "data/all_results.rds")
},
data.frame(i = 1),
add_objects = c("do_all_parameters",
                "parameters", "cpus",
                "working_directory",
                "chunks", "number_of_chunks",
                custom_functions),
jobname = 'Combine_files',
nodes = 1,
cpus_per_node = cpus,
slurm_options = sopt)
