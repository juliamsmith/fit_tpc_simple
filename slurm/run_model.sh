#!/bin/bash
#SBATCH --job-name=tpc_{MODEL}_{SPECIES}
#SBATCH --output=output/logs/%j.out
#SBATCH --error=output/logs/%j.err
#SBATCH --time=24:00:00
#SBATCH -p compute
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G



# Set working directory
WORKDIR=/mmfs1/gscratch/biology/jmsmith/fit_tpc_simple
RLIB=/gscratch/biology/jmsmith/R
cd $WORKDIR

module load apptainer/1.1.5
apptainer exec \
  --bind $WORKDIR:$WORKDIR \
  --bind $RLIB:$RLIB \
  tidyverse_latest.sif \
  Rscript $WORKDIR/scripts/fit_model.R {MODEL} {SPECIES}
