#!/bin/sh
#SBATCH -N 4
#SBATCH --ntasks-per-node 24
#SBATCH --output output.out
#SBATCH --partition shas
#SBATCH --time 5:00:00

module load intel
module load R
module load loadbalance
module load impi

mpirun lb /work/KellerLab/david/cotwins-analyses/src/location/standardize_location_data_lb.txt
