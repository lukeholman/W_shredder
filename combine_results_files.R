# Bash to sync Spartan files to local directory
# rsync -aPz lukeholman@spartan:/data/projects/punim0243/W_shredder/data/sim_results /Users/lholman/Documents/W_shredder/data

source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/model_functions.Rmd")

all_files <- list.files("data/sim_results", full.names = TRUE)
print(paste("About to merge", length(all_files), "files"))
all_files <- split(all_files, ceiling(seq_along(all_files) / 10000))
print(paste("Splitting them into", length(all_files), "chunks of up to 10,000"))

library(future)
library(future.apply)
options(mc.cores = 4)
plan("multicore")
future_lapply(1:length(all_files), combine_results_files, all_files = all_files, wd = getwd())





