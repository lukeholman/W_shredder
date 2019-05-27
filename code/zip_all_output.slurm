#!/bin/bash

#SBATCH --job-name=zip_all_files
#SBATCH -p cloud

#SBATCH --time=240:00:00

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

#SBATCH --mem=50000

#SBATCH --mail-user="luke.holman@unimelb.edu.au"
#SBATCH --mail-type=ALL


cd /data/projects/punim0243/W_shredder/data
module load R/3.4.0-GCC-4.9.2


# zip -r sim_results.zip sim_results

# zip -r sim_results_newest.zip $(find sim_results -type f -mtime -10)

