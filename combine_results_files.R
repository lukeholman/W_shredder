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

sopt <- list(time = '24:00:00',   # time in hours
             mem  = '32768')

all_files <- list.files("data/sim_results", full.names = TRUE)
all_files <- split(all_files, ceiling(seq_along(all_files) / 10^5))

sjob <- slurm_apply(
  f = function(i) {
    combine_results_files(vector_of_file_names = all_files[[i]],
                          wd = working_directory)
  },
  params = data.frame(i = 1:length(all_files)),
  add_objects = c("working_directory",
                  "all_files",
                  custom_functions),
  jobname = 'combine_files',
  nodes = length(all_files),
  cpus_per_node = 1,
  slurm_options = sopt
)
