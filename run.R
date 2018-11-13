# module load R/3.4.0-GCC-4.9.2
# cd /data/projects/punim0243/W_shredder

if(!dir.exists("data/sim_results")) dir.create("data/sim_results")

knitr::purl("analysis/run_model.Rmd")
source("run_model.R")
unlink("run_model.R")
