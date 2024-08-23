message('----- START DATA SIMULATION SCRIPT -----')
# Simulate data using one model function for all sets of parameters provided

#### LOAD PACKAGES -----
message('LOAD PACKAGES')
packages = c('optparse', 'dplyr', 'circular')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

#### PARSE BASH INPUT -----
message('PARSE BASH INPUT')
option_list = list(
  make_option(c('-c', '--codedir'),
              type='character',
              dest='codedir',
              help='Directory containing the code for the models',
              metavar = 'MODELS'),
  make_option(c('-d', '--data'),
              type='character',
              default = NA,
              dest='data',
              help='Directory where data is',
              metavar = 'DATA'),
  make_option(c('-o','--outdir'),
              type='character',
              dest='outdir',
              help='Directory where to save output',
              metavar = 'OUTPUT'),
  make_option(c('-m','--model'),
              type='character',
              dest='model',
              help='Type of model to run',
              metavar = 'MODEL'),
  make_option(c('--default'),
              type='integer',
              dest='default_value',
              help='Value assigned to the reject choice',
              metavar = 'DEFAULT_VALUE'),
  make_option(c('--len'),
              type='integer',
              dest='feature_len',
              help='Number of entries in the feature vector',
              metavar = 'FEATURE_LEN'),
  make_option(c('--var'),
              default = NULL,
              type='integer',
              dest='initvar',
              help='Initial variance setting for the model',
              metavar = 'INITVAR'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# # for testing:
# opt = list()
# opt$codedir = '...'
# opt$data = '...'
# opt$outdir = '...'
# opt$model = '1LR'
# opt$initvar = 5
# opt$default_value = 50
# opt$feature_len = 15

#### LOAD MODEL FUNCTIONS -----
message('LOAD MODEL FUNCTIONS')
t1 = Sys.time()
opt$model_script = ifelse(opt$model == '1LR_cond', '1LR',
                          ifelse(opt$model == '2LR_feat', '2LR',
                                 ifelse(opt$model == '1LR_condC', '1LR', 
                                        ifelse(opt$model == '1LR_condSD', '1LR', opt$model))))
model.sources = c('helper_funcs.R', 'model_init.R', 'encode_vector.R', 'encode_params.R', 'create_params.R', 'helper_funcs.R')
invisible(sapply(paste(opt$codedir, model.sources, sep = "/"), source)) 
model_func = source(paste(opt$codedir, paste0('model_', opt$model_script, '.R'), sep = '/'))[[1]]

#### SET PARAMETERS -----
message('SET PARAMETERS')

# LOAD FROM DATA FILE CREATED WITH RECOVER_PARAMS.R
params_permut = read.csv(file = file.path(opt$outdir, opt$model, 'params_sim','params_sim.csv'))

settings = list()
settings$paramnames = colnames(params_permut)[-c(1:2)]
settings$nparams = length(settings$paramnames)
settings$nparam_combos = nrow(params_permut)

#### GET DATA -----
message('SIMULATE DATA')
cat('model:', opt$model, ' param combos:', settings$nparam_combos, '\n')

df_train = create_data(feature_len = opt$feature_len, reps = 4, sd_slow = 30, nblocks = 8, nsub = settings$nparam_combos)

# add info on simulation parameters and model
df_train = left_join(df_train, params_permut, by = 'subject')

ids = unique(df_train$subject)
nsub = length(ids)
nblocks = length(unique(df_train$block))
ntrials = length(df_train$block) / nsub

#### SIMULATE CHOICES -----
message('\nSIMULATE CHOICES')

# for each set of parameters
for (i in 1:nrow(params_permut)) {
  # get data and params to simulate with
  data = subset(df_train, subject == ids[i])

  params_sim = encode_params(as.numeric(params_permut[i,-c(1:2)]),
                             initvar = opt$initvar,
                             default_value = opt$default_value,
                             feature_len = opt$feature_len,
                             model_name = opt$model,
                             simulate_choices = T)
  
  # simulate model choices
  if (opt$model %in% c('1LR', '2LR', '1LR_cond', '2LR_feat', '1LR_condC', '1LR_condSD')) {
    model = model_init(params = params_sim,
                       data = data)
    model = model_func(params = params_sim,
                       data = data,
                       model = model)
  } else {
    model = model_func(params = params_sim,
                       data = data)
  }
  dim(model$choice) = c(ntrials)
  dim(model$received_reward) = c(ntrials)
  
  # replace choices with model choices
  df_train$choice[df_train$subject == ids[i]] = model$choice
  df_train$received_reward[df_train$subject == ids[i]] = model$received_reward
  cat('.')
}

#### SAVE DATA -----
message('\nSAVE DATA')

dir.create(file.path(opt$outdir, opt$model, 'simulated_data'), recursive = T)
write.csv(df_train,
          file = paste(opt$outdir, opt$model, 'simulated_data', paste0('slownessprior_train_', opt$model, '.csv'), sep = '/'),
          row.names = FALSE)
