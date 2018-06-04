#!/bin/bash
#SBATCH -N 1
#SBATCH --time=07:00:00
#SBATCH --ntasks-per-node=5
#SBATCH --mail-user=jonathanped@gmail.com
#SBATCH --mail-type=end

module load R

Rscript pedoeem_jonathan_project5_bridges.r

cp LOUT.csv
