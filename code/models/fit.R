message('----- START MODEL FITTING SCRIPT -----')
set.seed(12345)
# Script that runs model fitting on the cluster for one participant. 
# need to set settings$max_LL below to decide whether: 
#   - max likelihood fitting (settings$max_LL = T) or
#   - max reward fitting (settings$max_LL = F)
# Runs NLOPT_GN_DIRECT_L for 10.000 iterations, resulting in one set of parameter fits

# Infrastructure should be there to run approximate fitting algorithms. 
# In that case need: 
# - set opt$iter as a largeish number (number of times model fitting is run)
# - set ITERATIONS in fit_wrapper to the same number (creates different starting values for model fitting)
# The script will save parameter output from all iterations in params_fit
# and best fit parameters from among all iterations in params_best.  
# For the current setup these will be identical. 

#### LOAD PACKAGES -----
message('LOAD PACKAGES')
packages = c('nloptr', 'optparse', 'dplyr', 'circular')
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
              help='Directory where data is',
              metavar = 'DATA'),
  make_option(c('-p','--paramdir'),
              type='character',
              dest='paramdir',
              help='Directory where parameter settings',
              metavar = 'OUTPUT'),
  make_option(c('-o','--outdir'),
              type='character',
              dest='outdir',
              help='Directory where to save output',
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
  make_option(c('-i','--iter'),
              type='integer',
              dest='iter',
              help='Number of iterations of model fitting (with different starting values).',
              metavar = 'ITER'),
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
# opt$paramdir = '...'
# opt$outdir = '...'
# opt$sub_id = '...'
# opt$model = '1LR'
# opt$iter = 1
# opt$initvar = 5
# opt$default_value = 50
# opt$feature_len = 15
# param_settings = create_params(model = opt$model, n_values = 1)
# param_settings = rbind(param_settings, param_settings)

#### LOAD MODEL FUNCTIONS -----
message('LOAD MODEL FUNCTIONS')
t1 = Sys.time()
opt$model_script = ifelse(opt$model == '1LR_cond', '1LR',
                          ifelse(opt$model == '2LR_feat', '2LR',
                                 ifelse(opt$model == '1LR_condC', '1LR', 
                                        ifelse(opt$model == '1LR_condSD', '1LR', opt$model))))
model.sources = c('helper_funcs.R', 'model_init.R', 'encode_vector.R', 'encode_params.R', 'loss_func.R', paste('model_', opt$model_script, '.R', sep = ''))
invisible(sapply(paste(opt$codedir, model.sources, sep = .Platform$file.sep), source)) # need to determine env for source? .GlobalEnv

#### LOAD DATA -----
message('LOAD DATA')
df_train = read.csv(opt$data)

# filter out data of subject
df_train = df_train %>% 
  filter(subject == opt$sub_id)

#### SETTINGS -----
message('SETTINGS')
settings = list()
# fitting algorithm and stopping rule
settings$opts = list(algorithm="NLOPT_GN_DIRECT_L", print_level = 3, maxeval=10000)

# IMPORTANT: run max likelihood fitting (T) or max reward fitting (F)
settings$max_LL = T
if (settings$max_LL) { # if max likelihood use participant choices
  settings$simulate_choices = F
} else { # if max reward simulate choices
  settings$simulate_choices = T
}

# get parameter starting vals
param_settings = read.csv(paste(opt$paramdir, paste0("param_settings_", opt$model, ".csv"), sep = .Platform$file.sep))
settings$nparams = dim(param_settings)[2]/3
settings$paramnamesx0 = grep('x0_', colnames(param_settings), value = T)

t2 = Sys.time()
cat('LOADING TIME:', difftime(t2, t1, units = "secs"), 'secs\n')

# EMPTY DATAFRAME TO SAVE HISTORY OF FITTING ----
# save values of parameters and sum neg. log_lik on every fitting iteration
fitting_history = data.frame()

# RUN FITTING ----
cat('RUN FITTING', 'model:', opt$model, ', participant:', opt$sub_id, '\n')
cat('Using model script: ', opt$model_script, '\n')
param_out = matrix(nrow = opt$iter, ncol = 2*settings$nparams+3)
colnames(param_out) = c('subject', 'model', 'LL', gsub('x0_', '', settings$paramnamesx0), settings$paramnamesx0)

# for each iteration
t1 = Sys.time()
for (i in 1:opt$iter) {
  # filter out settings of current iteration
  param_settings_i = param_settings[i,] 
  
  x0 = param_settings_i %>%
    select(starts_with("x0")) %>%
    as.numeric()
  ub = param_settings_i %>%
    select(starts_with("ub")) %>%
    as.numeric()
  lb = param_settings_i %>%
    select(starts_with("lb")) %>%
    as.numeric()
  
  # run model fitting
  res = nloptr(x0 = x0, ub = ub, lb = lb, opts = settings$opts,
               eval_f = loss_func, data = df_train, 
               initvar = opt$initvar, 
               default_value = opt$default_value, 
               feature_len = opt$feature_len,
               model_name = opt$model, 
               simulate_choices = settings$simulate_choices,
               max_LL = settings$max_LL)
  print(res$message)
  # append output
  param_out[i,1] = opt$sub_id
  param_out[i,2] = opt$model
  param_out[i,3] = res$objective
  param_out[i,4:(settings$nparams+3)] = res$solution
  param_out[i,(4+settings$nparams):(2*settings$nparams+3)] = x0
  cat('.')
}
print(res$termination_conditions)
# warnings() # print to log file
t2 = Sys.time()
cat('FITTING LOOP TIME:', difftime(t2, t1, units = "secs"), 'secs\n')


# GET BEST FIT PARAMETERS ----
message('GET BEST FIT PARAMETERS')
t1 = Sys.time()
# choose parameter fits with lowest sum negative LL 
param_best = data.frame(param_out) %>%
  filter(LL == min(LL)) %>%
  # distinct(subject, .keep_all = T) #, prior # if multiple solutions with same LL, keeps only one!
  distinct() # only removes completely identical rows

t2 = Sys.time()
cat('BEST PARAMS TIME:', difftime(t2, t1, units = "secs"), 'secs\n')

# SAVE OUTPUT ----
message('SAVE OUTPUT')
t1 = Sys.time()


param_out_file = paste0(paste('params', opt$model, opt$sub_id, sep = '_'),'.csv')
param_best_file = paste0(paste('params_best', opt$model, opt$sub_id, sep = '_'),'.csv')


write.csv(param_out,
          file = paste(opt$outdir, 'params_fit', param_out_file, sep = .Platform$file.sep),
          row.names = FALSE)
write.csv(param_best,
          file = paste(opt$outdir, 'params_best', param_best_file, sep = .Platform$file.sep),
          row.names = FALSE)

# save fitting history 
fitting_history_file = paste0(paste('fitting_history', opt$model, opt$sub_id, sep = '_'),'.csv')
write.csv(fitting_history,
          file = paste(opt$outdir, 'fitting_history', fitting_history_file, sep = .Platform$file.sep),
          row.names = FALSE)

t2 = Sys.time()
cat('SAVING TIME:', difftime(t2, t1, units = "secs"), 'secs\n')

cat('DONE FITTING', 'model:', opt$model, ', participant:', opt$sub_id, '\n\n')
