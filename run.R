# module load R/3.4.0-GCC-4.9.2
# cd /data/projects/punim0243/W_shredder

if(!dir.exists("data/sim_results")) dir.create("data/sim_results")

source_rmd <- function(file){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv())
}
source_rmd("analysis/run_model.Rmd")


