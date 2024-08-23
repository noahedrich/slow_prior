message('----- CREATE PARAMETER STARTING VALUES FOR FITTING -----')
# saves random parameter starting values for model fitting, between upper and lower bounds

#### LOAD PACKAGES -----
packages = c('dplyr',  'tidyr', 'optparse')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

#### PARSE BASH INPUT -----
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
              metavar = 'MODEL'),
  make_option(c('-n','--nvals'),
              type='integer',
              dest='nvals',
              help='Number of starting values to create',
              metavar = 'NVALS'))

# # for testing
# opt = NULL
# opt$codedir = '/Users/hedrich/Documents/slowness_prior/Models'
# opt$outdir = '/Users/hedrich/Documents/slowness_output/test'
# opt$model = '1LR'
# opt$nvals = 1

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

#### LOAD FUNCTION -----
source(paste(opt$codedir, 'create_params.R', sep = .Platform$file.sep))

#### RUN FUNCTION -----
filename = paste(opt$outdir, paste0("param_settings_", opt$model, ".csv"), sep = .Platform$file.sep)
if (!file.exists(filename)) {
  dir.create(file.path(opt$outdir), recursive = T)
  output = create_params(model = opt$model, n_values = opt$nvals)
  write.csv(output, file = filename, row.names = FALSE)
} 
