#!/bin/bash

#SBATCH -D /home/jsgriffi
#SBATCH --job-name=hert
#SBATCH --mem=4G
#SBATCH --ntasks=8
#SBATCH -o /home/jsgriffi/DS_maturation/output_files/out-%A.%a_hert.txt
#SBATCH -e /home/jsgriffi/DS_maturation/output_files/error-%A.%a_hert.txt
#SBATCH --time=176:00:00
#SBATCH --mail-user=jsgriffiths@ucdavis.edu
#SBATCH --mail-type=ALL
#SBATCH -p med

cd /home/jsgriffi/DS_maturation/R_scripts

module load R/3.6.3

Rscript maturation_glmmMCMC.R --save

date
exit


