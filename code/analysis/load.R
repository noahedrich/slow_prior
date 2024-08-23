# Loads all packages, data and helper functions for the analysis scripts
set.seed(12345)

# PACKAGES ----
# library(plyr)
library(here)
library(dplyr)
library(data.table)
library(tidyr)
library(lme4)
library(xtable)
library(meta)
library(ggtern)
library(bmsR)

# LOAD GGPLOT SETTINGS ----
source(here('analysis', 'ggplot_settings.R'))

# LOAD HELPER FUNCTIONS ----
source(here('models',  'helper_funcs.R'))

# SAVE LOCATION ----
save_path = here('..', 'figures')
if (!file.exists(save_path)) {
  dir.create(file.path(save_path), recursive = T)
}

# LOAD DATA ----
experiments = c('experiment1', 'experiment2', 'replication')
data_path = here('..',  'data_participants')
model_path = here('..',  'data_models')

DATA = data.frame()
DATA_TEST = data.frame()
MODELS = data.frame()
MODELS_TEST = data.frame()
MODELS_OPT = data.frame()
MODELS_TEST_OPT = data.frame()

for (experiment in experiments) {
  
  DATA_exp = as.data.frame(data.table::fread(paste(data_path, experiment,'slownessprior_train.csv',sep=.Platform$file.sep),header = TRUE))
  DATA_TEST_exp = as.data.frame(data.table::fread(paste(data_path, experiment,'slownessprior_test.csv',sep=.Platform$file.sep),header = TRUE))
  
  DATA = rbind(DATA, DATA_exp)
  DATA_TEST = rbind(DATA_TEST, DATA_TEST_exp)
  
  if (experiment %in% c('experiment2', 'replication')) {

    # LL models learning phase
    filenames = list.files(path=paste(model_path, experiment, 'LL', 'predictions', sep = .Platform$file.sep), pattern = '*.csv', full.names = T)
    l = lapply(filenames, read.csv)
    MODELS_exp = rbindlist(l, fill = T)
    MODELS_exp$experiment = experiment
    MODELS = rbind(MODELS, MODELS_exp)

    # LL models test phase
    filenames = list.files(path=paste(model_path, experiment, 'LL', 'predictions_test', sep = .Platform$file.sep), pattern = '*.csv', full.names = T)
    l = lapply(filenames, read.csv)
    MODELS_TEST_exp = rbindlist(l, fill = T)
    MODELS_TEST_exp$experiment = experiment
    MODELS_TEST = rbind(MODELS_TEST, MODELS_TEST_exp)

    # optimal models learning phase
    filenames = list.files(path=paste(model_path, experiment, 'optimal', 'predictions', sep = .Platform$file.sep), pattern = '*.csv', full.names = T)
    l = lapply(filenames, read.csv)
    MODELS_OPT_exp = rbindlist(l, fill = T)
    MODELS_OPT_exp$experiment = experiment
    MODELS_OPT = rbind(MODELS_OPT, MODELS_OPT_exp)

    # optimal models test phase
    filenames = list.files(path=paste(model_path, experiment, 'optimal', 'predictions_test', sep = .Platform$file.sep), pattern = '*.csv', full.names = T)
    l = lapply(filenames, read.csv)
    MODELS_TEST_OPT_exp = rbindlist(l, fill = T)
    MODELS_TEST_OPT_exp$experiment = experiment
    MODELS_TEST_OPT = rbind(MODELS_TEST_OPT, MODELS_TEST_OPT_exp)
  }
}

# PREPARE DATA ---- 
# participants ----
DATA = DATA %>% 
  filter(choice %in% c(0,1)) %>% # exclude no response trials
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')))
DATA_TEST = DATA_TEST %>%
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')))



# models ----
model_names = c('1LR', '2LR_F', '2LR_C', '4LR', 'WSLS', 'Bandit', 'Rd. Choice', 'Rd. Key', '1LR_C', '1LR_SD')
model_nparams = c(    4,       5,       5,     7,      1,        3,            1,         1,       5,        5)

MODELS = MODELS %>% 
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')),
         model = ifelse(model == '2LR', '4LR', model),
         model = ifelse(model == '1LR_cond', '2LR_C', model),
         model = ifelse(model == '2LR_feat', '2LR_F', model),
         model = ifelse(model == 'basic', 'Bandit', model),
         model = ifelse(model == 'random_choice', 'Rd. Choice', model),
         model = ifelse(model == 'random_key', 'Rd. Key', model), 
         model = ifelse(model == '1LR_condC', '1LR_C', model), 
         model = ifelse(model == '1LR_condSD', '1LR_SD', model), 
         model_received_reward = ifelse(p_accept>.5, rescaled_reward, 50),
         model = factor(model, levels = model_names)) %>%
  left_join(DATA %>% 
              select(subject, block, trial_number_block, received_reward)) %>%
  rename(part_received_reward = received_reward) 

MODELS_TEST = MODELS_TEST %>% 
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')),
         model = ifelse(model == '2LR', '4LR', model),
         model = ifelse(model == '1LR_cond', '2LR_C', model),
         model = ifelse(model == '2LR_feat', '2LR_F', model),
         model = ifelse(model == 'basic', 'Bandit', model),
         model = ifelse(model == 'random_choice', 'Rd. Choice', model),
         model = ifelse(model == 'random_key', 'Rd. Key', model), 
         model = ifelse(model == '1LR_condC', '1LR_C', model), 
         model = ifelse(model == '1LR_condSD', '1LR_SD', model), 
         model = factor(model, levels = model_names)) 

MODELS_OPT = MODELS_OPT %>% 
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')),
         model = ifelse(model == '2LR', '4LR', model),
         model = ifelse(model == '1LR_cond', '2LR_C', model),
         model = ifelse(model == '2LR_feat', '2LR_F', model),
         model = ifelse(model == 'basic', 'Bandit', model),
         model = ifelse(model == 'random_choice', 'Rd. Choice', model),
         model = ifelse(model == 'random_key', 'Rd. Key', model), 
         model = ifelse(model == '1LR_condC', '1LR_C', model), 
         model = ifelse(model == '1LR_condSD', '1LR_SD', model), 
         model_received_reward = ifelse(p_accept>.5, rescaled_reward, 50),
         model = factor(model, levels = model_names)) %>%
  left_join(DATA %>% 
              select(subject, block, trial_number_block, received_reward)) %>%
  rename(part_received_reward = received_reward) 

MODELS_TEST_OPT = MODELS_TEST_OPT %>% 
  mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast')),
         model = ifelse(model == '2LR', '4LR', model),
         model = ifelse(model == '1LR_cond', '2LR_C', model),
         model = ifelse(model == '2LR_feat', '2LR_F', model),
         model = ifelse(model == 'basic', 'Bandit', model),
         model = ifelse(model == 'random_choice', 'Rd. Choice', model),
         model = ifelse(model == 'random_key', 'Rd. Key', model), 
         model = ifelse(model == '1LR_condC', '1LR_C', model), 
         model = ifelse(model == '1LR_condSD', '1LR_SD', model), 
         model = factor(model, levels = model_names)) 
