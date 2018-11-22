#!/bin/bash

#SBATCH --job-name=set_up_parameters
#SBATCH --time=2:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=30720

module load R/3.4.0-GCC-4.9.2

Rscript /data/projects/punim0243/W_shredder/code/set_up_parameters.R
