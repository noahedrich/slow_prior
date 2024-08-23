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

# which model simulated data to use
MODEL_LIST=("1LR" "2LR" "1LR_cond" "2LR_feat" "basic" "random_choice" "random_key" "WSLS" "1LR_condC" "1LR_condSD") 
# which models to fit
MODEL_FIT_LIST=("1LR" "2LR" "1LR_cond" "2LR_feat" "basic" "random_choice" "random_key" "WSLS" "1LR_condC" "1LR_condSD")


# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/code"

# output directory
PATH_OUT="${HOME}/data_models/recover"

# param fittting start vals directory
PATH_PARAM_SETTINGS="${PATH_OUT}/param_settings"

# directory to save logs of HPC
PATH_LOG="${PATH_OUT}/logs/$(date '+%Y%m%d_%H%M')"

# Path to directory with script to run
PATH_CODE="${PATH_BASE}/models"


# create directory for log files:
if [ ! -d ${PATH_LOG} ]; then
mkdir -p ${PATH_LOG}
fi
# create param directory:
if [ ! -d ${PATH_PARAM_SETTINGS} ]; then
mkdir -p ${PATH_PARAM_SETTINGS}
fi

# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *GB*
MEM_MB=250
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"

# ====================================================
for MODEL in ${MODEL_LIST[@]}; do

# ===
# Get simulation list
# ===
# read subject IDs out of file and into array
readarray -t SIM_LIST < <(cut -d, -f1 "${PATH_OUT}/${MODEL}/params_sim/params_sim.csv")
# remove first emelent (column name)
SIM_LIST=("${SIM_LIST[@]:1}")
# keep only unique ids
IFS=" " read -r -a SIM_LIST <<< "$(tr ' ' '\n' <<< "${SIM_LIST[@]}" | sort -u | tr '\n' ' ')"


# Model specific paths
PATH_DATA_FILE="${PATH_OUT}/${MODEL}/simulated_data/slownessprior_train_${MODEL}.csv"
PATH_OUT_FOLDER="${PATH_OUT}/${MODEL}"
PATH_PARAM_FITS="${PATH_OUT_FOLDER}/params_fit"
PATH_PARAM_BEST="${PATH_OUT_FOLDER}/params_best"
PATH_OUT_HISTORY="${PATH_OUT_FOLDER}/fitting_history"

if [ ! -d ${PATH_OUT_FOLDER} ]; then
mkdir -p ${PATH_OUT_FOLDER}
fi
if [ ! -d ${PATH_PARAM_FITS} ]; then
mkdir -p ${PATH_PARAM_FITS}
fi
if [ ! -d ${PATH_PARAM_BEST} ]; then
mkdir -p ${PATH_PARAM_BEST}
fi
if [ ! -d ${PATH_OUT_HISTORY} ]; then
mkdir -p ${PATH_OUT_HISTORY}
fi

# run model fitting for each model
for MODEL_FIT in ${MODEL_FIT_LIST[@]}; do


# run model fitting for each parameter setting
for ITER in ${SIM_LIST[@]}; do

# Get job name
JOB_NAME="recov_${MODEL_FIT}_from_${MODEL}_${ITER}"

# Create job file
echo "#!/bin/bash" > job.slurm
# name of the job
echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
# set the expected maximum running time for the job:
echo "#SBATCH --time 03:00:00" >> job.slurm
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

# fit model to data
echo "Rscript ${PATH_CODE}/fit.R \
	-c ${PATH_CODE} \
	-d ${PATH_DATA_FILE} \
	-p ${PATH_PARAM_SETTINGS} \
	-o ${PATH_OUT_FOLDER} \
	-s ${ITER} \
	-m ${MODEL_FIT} \
	-i ${ITERATIONS} \
	--default ${DEFAULT_VALUE} \
	--var ${INITVAR} \
	--len ${FEATURE_LEN}" >> job.slurm


# submit job to cluster queue and remove it to avoid confusion:
sbatch job.slurm
rm -f job.slurm

done # end parameter setting loop

done # end model fitting loop

done # end model simulation loop
