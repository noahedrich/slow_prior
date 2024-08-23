message('----- START PARAMETER SIMULATION SCRIPT -----')
# create parameter values to simulate models with for parameter recovery

#### LOAD PACKAGES -----
message('LOAD PACKAGES')
packages = c('optparse', 'dplyr')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

#### PARSE BASH INPUT -----
message('PARSE BASH INPUT')
option_list = list(
  make_option(c('-c', '--codedir'),
              type='character',
              dest='codedir',
              help='Directory containing the code for the models',
              metavar = 'MODELS'),
  make_option(c('-o','--outdir'),
              type='character',
              dest='outdir',
              help='Directory where to save output',
              metavar = 'OUTPUT'),
  make_option(c('-m','--model'),
              type='character',
              dest='model',
              help='Type of model to run',
              metavar = 'MODEL'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# # for testing:
# opt = list()
# opt$codedir = '...'
# opt$outdir = '...'
# opt$model = '1LR'

#### LOAD FUNCTIONS -----
message('LOAD FUNCTIONS')
t1 = Sys.time()
model.sources = c('create_params.R')
invisible(sapply(paste(opt$codedir, model.sources, sep = .Platform$file.sep), source)) 

#### SET PARAMETERS -----
message('SET PARAMETERS')
settings = NULL
# get lower and upper bound of each parameter
# using this function so that have a central place to set bounds and paramnames
params = create_params(opt$model, 1)
settings$paramnames = gsub('x0_', '', grep('x0_', colnames(params), value = T))
settings$nparams = length(settings$paramnames)
lb = as.numeric(as.vector(params[grep('lb_', colnames(params))])) + 0.01
ub = as.numeric(as.vector(params[grep('ub_', colnames(params))])) - 0.01

# limit ranges for specific parameters
ub[settings$paramnames == 'kappa'] = 15
ub[settings$paramnames == 'c'] = 15
ub[settings$paramnames == 'c_slow'] = 15
ub[settings$paramnames == 'c_fast'] = 15
lb[settings$paramnames == 'choicesd'] = 15
lb[settings$paramnames == 'choicesd_slow'] = 15
lb[settings$paramnames == 'choicesd_fast'] = 15

# # OPTION 1: parameter values evenly spaced in these bounds
# n_values = 3 # number of different parameter values to run
# param_vals = lapply(seq_along(lb), function(i) seq(lb[i], ub[i], length.out = n_values))
# names(param_vals) = paste0(settings$paramnames, '_true')
# # set parameter values that want to be fixed
# # param_vals$choicesd = 2
# # get all permutations of the values
# params_permut = expand.grid(param_vals)
# params_permut = cbind(data.frame(model_true = opt$model), params_permut)
# params_permut = cbind(data.frame(subject = paste0('sim_', 1:nrow(params_permut))), params_permut)

# OPTION 2: randomly sampled parameters in the [lb - ub] range
n_simulations = 100
param_vals = lapply(seq_along(lb), function(i) runif(n_simulations, min = lb[i], max = ub[i]))
names(param_vals) = paste0(settings$paramnames, '_true')

params_permut = cbind(data.frame(model_true = opt$model), data.frame(param_vals))
params_permut = cbind(data.frame(subject = paste0('sim_', 1:nrow(params_permut))), params_permut)

# # OPTION 3: manually determined parameter values
# # kappa, c, choicesd, alpha ...
# if (opt$model == '1LR') {
#   params_permut = matrix(c(5, 0.1, 10, 0.1),nrow = 1)#matrix(c(5, 0.1, 10, 0.6, 5, 0.1, 10, 0.7), ncol = 4, byrow = T)
# } else if (opt$model == '1LR_cond') {
#   params_permut = matrix(c(5, 0.1, 10, 0.9, 0.1),nrow = 1)#matrix(c(5, 0.1, 10, 0.9, 0.7, 5, 0.1, 10, 0.7, 0.9), ncol = 5, byrow = T)#
# } else if (opt$model == '2LR') {
#   params_permut = matrix(c(5, 0.1, 10, 0.9, 0.2, 0.8, 0.3),nrow = 1)
# }

#### SAVE DATA -----
message('SAVE DATA')

dir.create(file.path(opt$outdir, opt$model, 'params_sim'), recursive = T)
write.csv(params_permut,
          file = paste(opt$outdir, opt$model, 'params_sim', 'params_sim.csv', sep = .Platform$file.sep),
          row.names = FALSE)
