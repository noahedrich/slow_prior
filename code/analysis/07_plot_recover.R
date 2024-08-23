# plot results of model recovery 
# careful: summarise dplyr vs plyr

# PATHS ----
library(plyr)
library(data.table)
library(dplyr)
library(here)
source(paste(here(), 'models',  'helper_funcs.R', sep = .Platform$file.sep))
recover_path = paste(here(), '..', 'data_models', 'recover', sep = .Platform$file.sep)
save_path = paste(here(), '..', 'figures', sep = .Platform$file.sep)
if (!file.exists(save_path)) {
  dir.create(file.path(save_path), recursive = T)
}

model_names = c('1LR', '2LR_feat', '1LR_cond', '2LR', 'WSLS', 'basic', 'random_choice', 'random_key', '1LR_condC', '1LR_condSD')
model_nparams = c(  4,          5,          5,     7,      1,        3,            1,         1,       5,        5)

# LOAD GGPLOT SETTINGS ----
source(paste(here(), 'analysis', 'ggplot_settings.R', sep = .Platform$file.sep))

# LOAD DATA ----
DATA_ALL = data.frame()
DATA_TRUE = data.frame()
DATA_RECOV = data.frame()
for (true_model in model_names) {
  cat('\ntrue model:', true_model)
  true_data_path = paste(recover_path, true_model, 'simulated_data', sep = .Platform$file.sep)
  data_true_model = read.csv(paste0(true_data_path, .Platform$file.sep, 'slownessprior_train_', true_model, '.csv'))
  DATA_TRUE = plyr::rbind.fill(DATA_TRUE, data_true_model)
  
  for (recov_model in model_names) {
    filenames_recov = list.files(path=paste(recover_path, true_model, 'params_best', sep = .Platform$file.sep), 
                                 pattern = paste0('params_best_', recov_model, '_sim.*.csv'), full.names = T)
    if (length(filenames_recov) == 0) {
      cat('skip:', recov_model)
      next
    }
    l = lapply(filenames_recov, read.csv)
    data_recov_model = rbindlist(l, fill = T)
    
    data_join = left_join(data_true_model, data_recov_model, by = 'subject')
    DATA_ALL = plyr::rbind.fill(DATA_ALL, data_join)
    
    data_recov_model$model_true = true_model
    DATA_RECOV = plyr::rbind.fill(DATA_RECOV, data_recov_model)
    cat('.')
  }
}

# LL variable in DATA_RECOV is the sum **negative** log likelihood across all trials used for model fitting
# for AICc need the positive LL, hence convert
DATA_RECOV$LL = -DATA_RECOV$LL

# parameter recovery ----
# plot correlation between true and recovered parameters (fit model == true model)
# only for the learning rates of models of interest 

# 1LR ----
plot1LR = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '1LR') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha, y = initalpha_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '1LR alpha')
plot1LR = ggExtra::ggMarginal(plot1LR, type = "density", fill = 'grey', col = NA)
plot1LR

# 2LR_C ----
plot1LR_cond_slow = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '1LR_cond') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_slow, y = initalpha_slow_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '2LR_C alpha_slow')
plot1LR_cond_slow = ggExtra::ggMarginal(plot1LR_cond_slow, type = "density", fill = 'grey', col = NA)
plot1LR_cond_slow

plot1LR_cond_fast = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '1LR_cond') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_fast, y = initalpha_fast_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '2LR_C alpha_fast')
plot1LR_cond_fast = ggExtra::ggMarginal(plot1LR_cond_fast, type = "density", fill = 'grey', col = NA)
plot1LR_cond_fast

# 2LR_F ----
plot2LR_feat_slow = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR_feat') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_slow, y = initalpha_slow_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '2LR_F alpha_slow')
plot2LR_feat_slow = ggExtra::ggMarginal(plot2LR_feat_slow, type = "density", fill = 'grey', col = NA)
plot2LR_feat_slow

plot2LR_feat_fast = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR_feat') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_fast, y = initalpha_fast_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '2LR_F alpha_fast')
plot2LR_feat_fast = ggExtra::ggMarginal(plot2LR_feat_fast, type = "density", fill = 'grey', col = NA)
plot2LR_feat_fast


# 4LR ----
plot2LR_slow_rel = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_slow_rel, y = initalpha_slow_rel_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '4LR alpha_slow_rel')
plot2LR_slow_rel = ggExtra::ggMarginal(plot2LR_slow_rel, type = "density", fill = 'grey', col = NA)
plot2LR_slow_rel

plot2LR_slow_irrel = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_slow_irrel, y = initalpha_slow_irrel_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '4LR alpha_slow_irrel')
plot2LR_slow_irrel = ggExtra::ggMarginal(plot2LR_slow_irrel, type = "density", fill = 'grey', col = NA)
plot2LR_slow_irrel

plot2LR_fast_rel = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_fast_rel, y = initalpha_fast_rel_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '4LR alpha_fast_rel')
plot2LR_fast_rel = ggExtra::ggMarginal(plot2LR_fast_rel, type = "density", fill = 'grey', col = NA)
plot2LR_fast_rel

plot2LR_fast_irrel = DATA_ALL %>%
  filter(model_true == model) %>%
  filter(model == '2LR') %>%
  distinct(subject, .keep_all = T) %>%
  select(starts_with('initalpha')) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  ggplot(aes(x = initalpha_fast_irrel, y = initalpha_fast_irrel_true)) +
  geom_smooth(method = 'lm', col = 'grey', alpha = alpha_ribbon) +
  geom_point(show.legend = F, col = 'black', size = 0.5) +
  coord_cartesian(xlim = lim_alpha, ylim = lim_alpha) +
  scale_x_continuous(breaks=breaks_alpha) +
  scale_y_continuous(breaks=breaks_alpha) +
  labs(x = 'fit learning rate', y = 'true learning rate', title = '4LR alpha_fast_irrel')
plot2LR_fast_irrel = ggExtra::ggMarginal(plot2LR_fast_irrel, type = "density", fill = 'grey', col = NA)
plot2LR_fast_irrel


parameter_recovery = list(plot1LR, plot1LR_cond_slow, plot1LR_cond_fast, plot2LR_feat_slow, plot2LR_feat_fast, plot2LR_slow_rel, plot2LR_slow_irrel, plot2LR_fast_rel, plot2LR_fast_irrel)  
for(i in 1:length(parameter_recovery)){
  ggsave(plot = parameter_recovery[[i]], file = paste(save_path, 'parameter_recovery_',i,'.svg', sep = .Platform$file.sep), width = 5, height = 5, dpi = 300, units = 'cm')
}


# plot the model recovery ----
# for each fit simulation calc the AICc
nparams = data.frame(model   = model_names,
                     nparams = model_nparams)

model_recov = DATA_RECOV %>%
  left_join(nparams, by = 'model') %>% 
  mutate(AICc = calc_AICc(LL= LL, nparams = nparams, nsample =  480)) %>%
  select(model_true, subject, model, AICc) %>%
  dplyr::group_by(model_true, subject) %>%
  dplyr::summarise(best_model = model[AICc == min(AICc)]) %>%
  group_by(model_true, best_model) %>%
  dplyr::summarise(n_matches = n())

model_recovery_plot = model_recov %>%
  dplyr::mutate(model_true = case_when(
    model_true == '2LR'          ~ '4LR',
    model_true == '1LR_cond'     ~ '2LR_C',
    model_true == '2LR_feat'     ~ '2LR_F',
    model_true == 'basic'        ~ 'Bandit',
    model_true == 'random_choice'~ 'Rd. Choice',
    model_true == 'random_key'   ~ 'Rd. Key',
    model_true == '1LR_condC'    ~ '1LR_C',
    model_true == '1LR_condSD'   ~ '1LR_SD',
    .default = model_true)) %>%
  dplyr::mutate(best_model = case_when(
    best_model == '2LR'          ~ '4LR',
    best_model == '1LR_cond'     ~ '2LR_C',
    best_model == '2LR_feat'     ~ '2LR_F',
    best_model == 'basic'        ~ 'Bandit',
    best_model == 'random_choice'~ 'Rd. Choice',
    best_model == 'random_key'   ~ 'Rd. Key',
    best_model == '1LR_condC'    ~ '1LR_C',
    best_model == '1LR_condSD'   ~ '1LR_SD',
    .default = best_model)) %>%
  dplyr::mutate(model_true = factor(model_true, levels = c('1LR', '2LR_F', '2LR_C', '4LR',  'WSLS', 'Bandit', 'Rd. Choice', 'Rd. Key', '1LR_C', '1LR_SD')),
         best_model = factor(best_model, levels = c('1LR', '2LR_F', '2LR_C', '4LR',  'WSLS', 'Bandit', 'Rd. Choice', 'Rd. Key', '1LR_C', '1LR_SD'))) %>%
  ggplot(aes(x = best_model, y = model_true, fill = n_matches))+
  geom_tile() +
  labs(x='fit model', y = 'true model', fill = '% best fit') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  # scale_fill_continuous(limits = c(1,100), breaks = c(1, 50, 100))
  scale_fill_gradientn(colours = viridis(100, option = "D"),
                       limits = c(1,100), breaks = c(1, 50, 100))
model_recovery_plot
ggsave(filename = paste(save_path, 'model_recovery.svg', sep = .Platform$file.sep), plot = model_recovery_plot, width = 9, height = 7, dpi = 300, units = 'cm')
