# Simulate the four learning models, to test whether they reproduce 
# slow > fast reward effect in the learning phase

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))
source(here('models', 'encode_params.R'))
source(here('models', 'encode_vector.R'))
source(here('models', 'model_init.R'))
source(here('models', 'model_1LR.R'))
source(here('models', 'model_2LR.R'))

# SETTINGS ----
simulated_model_names = c("1LR", "2LR", "1LR_cond", "2LR_feat")

alpha = 0.3
slow_bonus = 0.3
rel_bonus = 0.1

kappa = 3
c = 3
choicesd = 50

initvar=5
default_value=50
feature_len=15

# participant choices not used, just 
data_2_simulate = as.data.frame(data.table::fread(paste(data_path, 'experiment2','slownessprior_train.csv',sep=.Platform$file.sep),header = TRUE))
subs = unique(data_2_simulate$subject)
nblocks = max(data_2_simulate$block)
ntrials = max(data_2_simulate$trial_number_block)

simulation_results = data.frame()

# for each participant in experiment 2
for (sub_id in subs) {
  # get the data
  data = data_2_simulate %>% filter(subject == sub_id)
  
  # for each model
  for (model_name in simulated_model_names) {
    # set up parameters and model_func
    x = c(kappa, c, choicesd)
    if (model_name == "2LR") {
      x = c(x, alpha+slow_bonus+rel_bonus, alpha+slow_bonus, alpha+rel_bonus, alpha)
      model_func = model_2LR
    } else if (model_name == "2LR_feat") {
      x = c(x, alpha+slow_bonus, alpha)
      model_func = model_2LR
    } else if (model_name == "1LR_cond") {
      x = c(x, alpha+slow_bonus, alpha)
      model_func = model_1LR
    } else { #"1LR"
      x = c(x, alpha)
      model_func = model_1LR
    }
    
    # simulate model with the parameters
    params = encode_params(x, initvar, default_value, feature_len, model_name, simulate_choices=T)
    model = model_init(params = params, data = data)
    model = model_func(params = params, data = data, model = model)
    
    # save simulation results
    res = matrix(NA, nblocks*ntrials, 6)
    res[,1] = sub_id
    res[,2] = model_name
    res[,3] = data$block
    res[,4] = data$trial_number_block
    res[,5] = data$prior
    res[,6] = model$received_reward # learning phase reward
    
    simulation_results = rbind(simulation_results, res)
    cat('.')
  }
  cat('\n')
}

colnames(simulation_results) = c('subject', 'model', 'block', 'trial_number_block', 'prior', 'received_reward')


# PLOT CUMULATIVE REWARD ----
simulated_LP_reward_boxplot = simulation_results %>%
  group_by(model, subject, prior, block) %>%
  mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
  summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
  summarise(received_reward_cumsum = mean(received_reward_cumsum)) %>%
  dplyr::mutate(model = case_when(
    model == '2LR'          ~ '4LR',
    model == '1LR_cond'     ~ '2LR_C',
    model == '2LR_feat'     ~ '2LR_F',
    .default = model)) %>%
  dplyr::mutate(model = factor(model, levels = c('1LR', '2LR_F', '2LR_C', '4LR'))) %>%
  dplyr::mutate(prior = ifelse(prior == 'congruent', 'slow', 'fast'),
         prior = factor(prior, levels = c('slow', 'fast'))) %>%
  ggplot(aes(x = prior, y = received_reward_cumsum, fill = prior)) +
  facet_wrap(~model, nrow = 1) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_point(alpha = alpha_individs, col = 'grey', show.legend = F) + 
  geom_line(aes(group = subject), alpha = alpha_individs,col = 'grey', show.legend = F) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F, outlier.colour = NA) + 
  labs(x = prior_label, y= 'cum. reward - chance') +
  scale_fill_manual(values = prior_palette) +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = lim_cum_reward, breaks = c(0, 200, 400, 600))
simulated_LP_reward_boxplot

ggsave(filename = paste(save_path, 'models_simulated_LP_reward_boxplot.svg', sep = .Platform$file.sep), 
       plot = simulated_LP_reward_boxplot, width = save_width, height = save_height, dpi = dpi, units = units, device = 'svg') 
