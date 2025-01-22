#!/bin/bash
#SBATCH -p general
#SBATCH -N 1
#SBATCH -t 00-02:00:00
#SBATCH --mem=75g
#SBATCH -n 1
#SBATCH --job-name=redundancy
#SBATCH --array=1-1980

# Load R module
module load r/3.6.0

# Define R script path
R_SCRIPT="/work/users/j/m/jmcard/redundancy/script/redundancy.R"

# Get the list of files in the folder
FILES=($(ls /work/users/j/m/jmcard/redundancy/travel_matrices))

# Get the file corresponding to this Slurm task ID
FILE=${FILES[$SLURM_ARRAY_TASK_ID-1]}

# Run R script with the selected file as an argument
Rscript $R_SCRIPT $FILE