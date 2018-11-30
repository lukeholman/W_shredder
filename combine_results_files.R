source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")
custom_functions <- ls()

# This bit is for the unimelb cluster, Spartan
working_directory <- "/data/projects/punim0243/W_shredder"
setwd(working_directory)

cpus <- 1
sopt <- list(time = '4:00:00',   # time in hours
             mem  = '51200')     # 50GB ram


sjob <- slurm_apply(
  f = function(i) {
    saveRDS(combine_results_files(cores = cpus,
                                  wd = working_directory),
            file = "/data/projects/punim0243/W_shredder/data/all_results.rds")},
  params = data.frame(i = 1),
  add_objects = c("cpus",
                  "working_directory",
                  custom_functions),
  jobname = 'combine_files',
  nodes = 1,
  cpus_per_node = cpus,
  slurm_options = sopt
)
