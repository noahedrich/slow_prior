#!/bin/bash

# ===
# Prepare experiment specific variables
# ===
# which data to use
EXP_FOLDER="replication"

# how many iterations of model fitting with different starting values
# if increase this need to increase number of random starting values (param settings)
ITERATIONS=1
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

# data directory
PATH_DATA="${HOME}/data_participants"

# output directory
PATH_OUT="${HOME}/data_models"

# directory to save logs of HPC
PATH_LOG="${PATH_OUT}/logs/$(date '+%Y%m%d_%H%M')"

# Path to directory with script to run
PATH_CODE="${PATH_BASE}/models"

# Experiment specific paths
PATH_DATA_FILE="${PATH_DATA}/${EXP_FOLDER}/slownessprior_train.csv"
PATH_DATA_FILE_TEST="${PATH_DATA}/${EXP_FOLDER}/slownessprior_test.csv"
PATH_OUT_FOLDER="${PATH_OUT}/${EXP_FOLDER}/LL" # or optimal

PATH_PARAM_SETTINGS="${PATH_OUT_FOLDER}/param_settings"
PATH_PARAM_FITS="${PATH_OUT_FOLDER}/params_fit"
PATH_PARAM_BEST="${PATH_OUT_FOLDER}/params_best"
PATH_OUT_PRED="${PATH_OUT_FOLDER}/predictions"
PATH_OUT_PRED_TEST="${PATH_OUT_FOLDER}/predictions_test"
PATH_OUT_HISTORY="${PATH_OUT_FOLDER}/fitting_history"

# ===
# Create directories
# ===
# create output directory:
if [ ! -d ${PATH_OUT_FOLDER} ]; then
mkdir -p ${PATH_OUT_FOLDER}
fi

# create subdirectory for parameter starting values:
if [ ! -d ${PATH_PARAM_SETTINGS} ]; then
mkdir -p ${PATH_PARAM_SETTINGS}
fi

# create subdirectory for parameter fits from all iterations:
if [ ! -d ${PATH_PARAM_FITS} ]; then
mkdir -p ${PATH_PARAM_FITS}
fi

# create output subdirectory for best parameter fits: 
if [ ! -d ${PATH_PARAM_BEST} ]; then
mkdir -p ${PATH_PARAM_BEST}
fi

# create output subdirectory to store model training trial predictions:
if [ ! -d ${PATH_OUT_PRED} ]; then
mkdir -p ${PATH_OUT_PRED}
fi

# create output subdirectory to store model test trial predictions:
if [ ! -d ${PATH_OUT_PRED_TEST} ]; then
mkdir -p ${PATH_OUT_PRED_TEST}
fi

# create output subdirectory to store history of each iteration of model fitting:
if [ ! -d ${PATH_OUT_HISTORY} ]; then
mkdir -p ${PATH_OUT_HISTORY}
fi


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
MEM_MB=180
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"


# ===
# Get participant list
# ===
# read subject IDs out of file and into array
readarray -t PARTICIPANT_LIST < <(cut -d, -f1 ${PATH_DATA_FILE})
# remove first emelent (column name)
PARTICIPANT_LIST=("${PARTICIPANT_LIST[@]:1}")
# keep only unique ids
IFS=" " read -r -a PARTICIPANT_LIST <<< "$(tr ' ' '\n' <<< "${PARTICIPANT_LIST[@]}" | sort -u | tr '\n' ' ')"

# ====================================================
for MODEL in ${MODEL_LIST[@]}; do

# run model fitting for each participant
for PARTICIPANT_ID in ${PARTICIPANT_LIST[@]}; do

# Get job name
JOB_NAME="fitting_${MODEL}_${PARTICIPANT_ID}"

# Create job file
echo "#!/bin/bash" > job.slurm
# name of the job
echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
# set the expected maximum running time for the job:
echo "#SBATCH --time 3:00:00" >> job.slurm
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

# create parameter starting values if not yet available
echo "Rscript ${PATH_CODE}/create_params_call.R \
	-c ${PATH_CODE} \
	-o ${PATH_PARAM_SETTINGS} \
	-m ${MODEL} \
	-n ${ITERATIONS}" >> job.slurm

# fit model to data
echo "Rscript ${PATH_CODE}/fit.R \
	-c ${PATH_CODE} \
	-d ${PATH_DATA_FILE} \
	-p ${PATH_PARAM_SETTINGS} \
	-o ${PATH_OUT_FOLDER} \
	-s ${PARTICIPANT_ID} \
	-m ${MODEL} \
	-i ${ITERATIONS} \
	--default ${DEFAULT_VALUE} \
	--var ${INITVAR} \
	--len ${FEATURE_LEN}" >> job.slurm

# predict choices on all data
echo "Rscript ${PATH_CODE}/predict.R \
	-c ${PATH_CODE} \
	-d ${PATH_DATA_FILE} \
	-t ${PATH_DATA_FILE_TEST} \
	-o ${PATH_OUT_FOLDER} \
	-p ${PATH_PARAM_BEST} \
	-s ${PARTICIPANT_ID} \
	-m ${MODEL} \
	--default ${DEFAULT_VALUE} \
	--var ${INITVAR} \
	--len ${FEATURE_LEN}" >> job.slurm

# submit job to cluster queue and remove it to avoid confusion:
sbatch job.slurm
rm -f job.slurm

done # end participant loop 

done # end model loop
