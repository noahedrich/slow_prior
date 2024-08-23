#!/bin/bash

# ===
# Prepare experiment specific variables
# ===

# how many iterations of model fitting with different starting values
# if increase this need to increase number of random starting values (param settings)
ITERATIONS=1
# settings for models
INITVAR=5
DEFAULT_VALUE=50
FEATURE_LEN=15

# which models to run 
MODEL_LIST=("1LR" "2LR" "1LR_cond" "2LR_feat" "basic" "random_choice" "random_key" "WSLS" "1LR_condC" "1LR_condSD") 


# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/code"

# output directory
PATH_OUT="${HOME}/data_models/recover"

# param fitting start vals directory
PATH_PARAM_SETTINGS="${PATH_OUT}/param_settings"

# directory to save logs of HPC
PATH_LOG="${PATH_OUT}/logs/$(date '+%Y%m%d_%H%M')"

# Path to directory with script to run
PATH_CODE="${PATH_BASE}/models"


# create directory for log files:
if [ ! -d ${PATH_LOG} ]; then
mkdir -p ${PATH_LOG}
fi

# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *GB*
MEM_MB=360
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"


# ====================================================
for MODEL in ${MODEL_LIST[@]}; do

# Get job name
JOB_NAME="PARAMS_${MODEL}"

# Create job file
echo "#!/bin/bash" > job.slurm
# name of the job
echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
# set the expected maximum running time for the job:
echo "#SBATCH --time 00:30:00" >> job.slurm
# determine how much RAM your operation needs:
echo "#SBATCH --mem ${MEM_MB}MB" >> job.slurm
# determine number of CPUs
echo "#SBATCH --cpus-per-task ${N_CPUS}" >> job.slurm
# write to log folder/
echo "#SBATCH --output ${PATH_LOG}/slurm-${JOB_NAME}.%j.out" >> job.slurm
# set working directory
echo "#SBATCH --chdir ${PATH_BASE}" >> job.slurm

# Load R module
echo "module unload R" >> job.slurm
echo "module load R/4.0" >> job.slurm

# create parameters to simulate models with
echo "Rscript ${PATH_CODE}/recover_params.R \
	-c ${PATH_CODE} \
	-o ${PATH_OUT} \
	-m ${MODEL} " >> job.slurm

# Run simulation of data for all parameters settings using one model
echo "Rscript ${PATH_CODE}/simulate.R \
	-c ${PATH_CODE} \
	-o ${PATH_OUT} \
	-m ${MODEL} \
	--default ${DEFAULT_VALUE} \
	--var ${INITVAR} \
	--len ${FEATURE_LEN}" >> job.slurm

# create parameter fitting starting values for model if not yet available
echo "Rscript ${PATH_CODE}/create_params_call.R \
	-c ${PATH_CODE} \
	-o ${PATH_PARAM_SETTINGS} \
	-m ${MODEL} \
	-n ${ITERATIONS}" >> job.slurm

# submit job to cluster queue and remove it to avoid confusion:
sbatch job.slurm
rm -f job.slurm

done # end model loop
