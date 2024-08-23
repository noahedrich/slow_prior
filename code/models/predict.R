message('----- START MODEL PREDICTION SCRIPT -----')
set.seed(12345)
# Script that runs model function using the best fit parameters from model fitting
# for one participant. Saves trial-wise model predictions for learning and test phases.

# need to set ssettings$simulate_choices below to decide whether: 
#   - use the choices in the data (presumably participant choices) (settings$simulate_choices = T) or
#   - let the model simulate its own choices (settings$simulate_choices = F)

#### LOAD PACKAGES -----
message('LOAD PACKAGES')
packages = c('optparse', 'dplyr', 'circular', 'tidyr')
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
              dest='data',
              help='File where data from training phase is',
              metavar = 'DATA'),
  make_option(c('-t', '--data_test'),
              type='character',
              dest='data_test',
              help='File where data from test phase is',
              metavar = 'DATA_TEST'),
  make_option(c('-o','--outdir'),
              type='character',
              dest='outdir',
              help='Directory where to save output',
              metavar = 'OUTPUT'),
  make_option(c('-p','--paramdir'),
              type='character',
              dest='paramdir',
              help='Directory where best fit params are saved',
              metavar = 'OUTPUT'),
  make_option(c('-s','--sub_id'),
              type='character',
              dest='sub_id',
              help='Subject ID',
              metavar = 'SUBJECT'),
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

# # for testing
# opt = NULL
# opt$codedir = '...'
# opt$data = '...'
# opt$data_test = '...'
# opt$paramdir = '...'
# opt$outdir = '...'
# opt$sub_id = '...'
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
model.sources = c('helper_funcs.R', 'model_init.R', 'encode_vector.R', 'encode_params.R')
invisible(sapply(paste(opt$codedir, model.sources, sep = .Platform$file.sep), source))
model_func = source(paste(opt$codedir, paste('model_', opt$model_script, '.R', sep = ''), sep = .Platform$file.sep))[[1]]

#### LOAD DATA -----
message('LOAD DATA')
df_train = read.csv(opt$data)

df_test = read.csv(opt$data_test)

# filter out data of subject
df_train = df_train %>% 
  filter(subject == opt$sub_id)
df_test = df_test %>% 
  filter(subject == opt$sub_id)

#### SETTINGS -----
message('SETTINGS')
settings = list()
# data size
settings$nblocks = length(unique(df_train$block))
settings$ntrials = length(unique(df_train$trial_number_block))
settings$ntrials_test = length(unique(df_test$pair_number_block))

#### GET TEST STIMULI -----
message('GET TEST STIMULI')
# get stimuli positions - put slow feature first and fast feature second
# dimensions: prior, trial, block
test_stimuli = NULL
test_stimuli$S_left = test_stimuli$S_right = array(NA, dim = c(2, settings$ntrials_test, settings$nblocks))
test_stimuli$S_left[1,,] = ifelse(df_test$slow == "color", df_test$pos_color_left, df_test$pos_shape_left)
test_stimuli$S_right[1,,] = ifelse(df_test$slow == "color", df_test$pos_color_right, df_test$pos_shape_right)
test_stimuli$S_left[2,,] = ifelse(df_test$slow == "color", df_test$pos_shape_left, df_test$pos_color_left)
test_stimuli$S_right[2,,] = ifelse(df_test$slow == "color", df_test$pos_shape_right, df_test$pos_color_right)

#### LOAD PARAMETERS -----
message('LOAD PARAMETERS')
filename = paste0(paste('params_best', opt$model, opt$sub_id, sep = '_'), '.csv')
paramfits = read.csv(file = paste(opt$paramdir, filename, sep = .Platform$file.sep))
settings$paramnames = gsub('x0_', '', grep('x0_', colnames(paramfits), value = T))
paramfits = paramfits %>% select(settings$paramnames)
settings$nparams = length(settings$paramnames)

t2 = Sys.time()
cat('LOADING TIME:', difftime(t2, t1, units = "secs"), 'secs\n')

#### RUN MODEL -----
message('RUN MODEL')
# IMPORTANT: simulate choices using model (T) or clamp to participant choices (F)
settings$simulate_choices = F
  
t1 = Sys.time()
# save training trials predictions
res = matrix(NA, settings$nblocks*settings$ntrials, 25+settings$nparams)
res[,1] = opt$sub_id
res[,2] = opt$model
res[,3] = df_train$block
res[,4] = df_train$trial_number_block
res[,5] = df_train$prior

# save test trials predictions
res_test = matrix(NA, settings$nblocks*settings$ntrials_test, 15+settings$nparams)
res_test[,1] = opt$sub_id
res_test[,2] = opt$model
res_test[,3] = df_test$block
res_test[,4] = df_test$pair_number_block
res_test[,5] = df_test$prior


# run model with paramfits and data - get training trial predictions
params = encode_params(paramfits, 
                       initvar = opt$initvar, 
                       default_value = opt$default_value, 
                       feature_len = opt$feature_len, 
                       model_name = opt$model,
                       simulate_choices = settings$simulate_choices)

if(params$random | params$basic | params$WSLS){ # random models + basic Kalman Filter + WSLS
  model = model_func(params = params, data = df_train)
} else { # actual testing models
  model = model_init(params = params, data = df_train)
  model = model_func(params = params, data = df_train, model = model)
}

# learning phase predictions ----
res[,6] = model$PE
res[,7] = model$V
res[,8] = model$V_slow
res[,9] = model$V_fast
res[,10] = model$Var
res[,11] = model$Var_slow
res[,12] = model$Var_fast
res[,13] = model$p_accept
res[,14] = model$p_choice
res[,15] = model$LL
res[,16] = model$choice
res[,17] = model$correct
res[,18] = df_train$choice # participant choice
res[,19] = df_train$correct # participant correct?
res[,20] = df_train$correct_choice # true correct choice
res[,21] = df_train$rescaled_reward # true stimulus reward

# save updated learning rates on every trial for 2LR model
if(params$two_LR & !params$feat_LR) { # 2LR
  res[res[,5] == 'congruent',  22] = c(model$alphas[1, 1:settings$ntrials,])[res[,5] == 'congruent'] # alpha_slow_rel
  res[res[,5] == 'incongruent',23] = c(model$alphas[1, 1:settings$ntrials,])[res[,5] == 'incongruent'] # alpha_slow_irrel
  res[res[,5] == 'incongruent',24] = c(model$alphas[16, 1:settings$ntrials,])[res[,5] == 'incongruent'] # alpha_fast_rel
  res[res[,5] == 'congruent',  25] = c(model$alphas[16, 1:settings$ntrials,])[res[,5] == 'congruent'] # alpha_fast_irrel
} 

res[,26:(25+settings$nparams)] = rep(as.numeric(as.vector(paramfits)), each = settings$nblocks*settings$ntrials)

# test phase predictions ----
if(params$random | params$basic | params$WSLS){ # random models + basic Kalman Filter + WSLS
  res_test[,6] = NA #V_left
  res_test[,7] = NA #V_right
  if (params$random & !params$choice) {# random key model
    res_test[,8] = rbinom(settings$ntrials_test*settings$nblocks, 1, params$bias_right) # model choice with bias to response key
  } else {
    res_test[,8] = rbinom(settings$ntrials_test*settings$nblocks, 1, .5) # model choice random, or bandit model, or WSLS model
  }
} else { # actual testing models
  # convert stimuli positions to von Mises distributions
  test_stimuli$X_left  = apply(test_stimuli$S_left[,,unique(df_test$block)],  c(2,3), function(x) encode_vector(x, params$feature_len, params$kappa))
  test_stimuli$X_right = apply(test_stimuli$S_right[,,unique(df_test$block)], c(2,3), function(x) encode_vector(x, params$feature_len, params$kappa))
  
  test_stimuli$V_left = matrix(NA, settings$ntrials_test, settings$nblocks)
  test_stimuli$V_right = matrix(NA, settings$ntrials_test, settings$nblocks)
  # use last trial value estimates to get estimated value
  for (b in 1:length(unique(df_test$block))) {
    test_stimuli$V_left[,b]  = apply(test_stimuli$X_left[,,b],  2, function(x) x %*% model$W[,settings$ntrials,b])
    test_stimuli$V_right[,b] = apply(test_stimuli$X_right[,,b], 2, function(x) x %*% model$W[,settings$ntrials,b])
  }
  res_test[,6] = test_stimuli$V_left
  res_test[,7] = test_stimuli$V_right
  res_test[,8] = (test_stimuli$V_left < test_stimuli$V_right)*1 # model choice: left larger = 0; right larger = 1
}

# participant and experiment data
res_test[,9] = (res_test[,8] == df_test$correct_index)*1 # model correct?
res_test[,10] = ifelse(df_test$key_press == 70, 0, 1) # participant choice
res_test[,11] = df_test$correct # participant correct?
res_test[,12] = df_test$rescaled_reward_left
res_test[,13] = df_test$rescaled_reward_right
res_test[,14] = df_test$rescaled_reward_diff
res_test[,15] = df_test$correct_index # true correct choice
res_test[,16:(15+settings$nparams)] = rep(as.numeric(as.vector(paramfits)), each = settings$nblocks*settings$ntrials_test)


colnames(res) = c('subject', 'model', 'block', 'trial_number_block', 'prior', 
                  'PE', 'V', 'V_slow', 'V_fast', 'Var', 'Var_slow', 'Var_fast', 'p_accept', 'p_choice', 'LL', 'model_choice', 'model_correct', 
                  'part_choice', 'part_correct', 'correct_choice', 'rescaled_reward', 'alpha_slow_rel', 'alpha_slow_irrel', 'alpha_fast_rel', 'alpha_fast_irrel', settings$paramnames)

colnames(res_test) = c('subject', 'model', 'block', 'trial_number_block', 'prior', 
                       'V_left', 'V_right', 'model_choice', 'model_correct', 
                       'part_choice', 'part_correct', 'reward_left', 'reward_right', 'reward_diff', 'correct_choice', settings$paramnames)

t2 = Sys.time()
cat('MODEL RUN TIME:', difftime(t2, t1, units = "secs"), 'secs\n')

#### SAVE RESULTS -----
message('SAVE RESULTS')
t1 = Sys.time()
filename = paste0(paste('predict', opt$model, opt$sub_id, sep = '_'), '.csv')
outfolder = 'predictions'
write.csv(res, file = paste(opt$outdir, outfolder, filename, sep = .Platform$file.sep),
          row.names = FALSE)

filename = paste0(paste('predict_test', opt$model, opt$sub_id, sep = '_'), '.csv')
outfolder = 'predictions_test'
write.csv(res_test, file = paste(opt$outdir, outfolder, filename, sep = .Platform$file.sep),
          row.names = FALSE)

t2 = Sys.time()
cat('SAVING TIME:', difftime(t2, t1, units = "mins"), 'mins\n')


cat('DONE', 'model:', opt$model, ', participant:', opt$sub_id, '\n\n')

# warnings() # print to log file
