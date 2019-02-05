#!/bin/bash

#SBATCH --job-name=zip_files
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=50000
cd /data/projects/punim0243/W_shredder/data
tar -zcvf sim_results.tgz sim_results
