#!/bin/bash
#SBATCH -p general
#SBATCH -N 1
#SBATCH -t 00-01:20:00
#SBATCH --mem=80g
#SBATCH -n 1
#SBATCH --job-name=redundancy
#SBATCH --array=1-7060

#load R module
module load r/4.4.0

#define R script path
R_SCRIPT="redundancy.R"

#get the list of files in the folder
FILES=($(TRAVEL_MATRIX_FOLDER))

#get the file corresponding to this Slurm task ID
FILE=${FILES[$SLURM_ARRAY_TASK_ID-1]}

#run R script with the selected file as an argument
Rscript $R_SCRIPT $FILE
