# Modelling Analysis
# Evaluate the results of model fitting 
# - Model fit curves (correct, accept, sensitivity)
# 1. Calculate AICc scores 
# 2. Caclulate exceedance probabilities
# - Bar plot
# - Simplex 
# 3. Cumulative reward of winning model in slow vs. fast condition
# 4. correlation relative fit of models vs. 1LR model and slowness effect
# 5. 4LR best fit learning rates
# 6. correlation between learning rates and behavioural effect

# !! make sure to set WD to the folder of this file before running, so that here() works
# !! here() should point to the 'code' folder

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))

# filter out one experiment sample
experiment_name = 'replication'
DATA = DATA %>% filter(experiment == experiment_name)
DATA_TEST = DATA_TEST %>% filter(experiment == experiment_name)
MODELS = MODELS %>% filter(experiment == experiment_name)
MODELS_TEST = MODELS_TEST %>% filter(experiment == experiment_name)
MODELS_OPT = MODELS_OPT %>% filter(experiment == experiment_name)
MODELS_TEST_OPT = MODELS_TEST_OPT %>% filter(experiment == experiment_name)

# remove models not in main paper? ---- 
MODELS = MODELS %>%
  filter(!model %in% c('1LR_C', '1LR_SD'))

MODELS_TEST = MODELS_TEST %>%
  filter(!model %in% c('1LR_C', '1LR_SD'))

MODELS_OPT = MODELS_OPT %>%
  filter(!model %in% c('1LR_C', '1LR_SD')) 

MODELS_TEST_OPT = MODELS_TEST_OPT %>%
  filter(!model %in% c('1LR_C', '1LR_SD'))

nparams = data.frame(model   = model_names,
                     nparams = model_nparams) %>%
  mutate(model = factor(model, levels = model_names))

nmodels = length(unique(MODELS$model))


# 1. Calculate AICc scores -----
baseline_model = '1LR'

model_comparison = MODELS %>% 
  group_by(model, subject) %>%
  summarise(LL = sum(LL, na.rm = T),
            N_AICc = sum(part_choice != -1)) %>% # n trials used for LL
  left_join(nparams, by = 'model') %>% 
  ungroup() %>%
  mutate(AICc = calc_AICc(LL, nparams, N_AICc)) %>%
  group_by(subject) %>%
  mutate(AICc_delta = AICc - AICc[model == baseline_model],
         AICc_weight = exp(-.5*AICc_delta) / sum(exp(-.5*AICc_delta)),
         best_model = model[AICc == min(AICc)]) %>%
  ungroup() %>%
  mutate(model = factor(model, levels = model_names),
         best_model = factor(best_model, levels = model_names))

# 2. Calculate exceedance probabilities ----
model_evidences = model_comparison %>%
  select(subject, model, AICc_weight) %>%
  pivot_wider(names_from = model, values_from = AICc_weight) %>%
  select(-subject) %>%
  data.matrix()
m = log(model_evidences)
bms0 = VB_bms(m)

XP = data.frame(
  model = colnames(m), 
  dirichlet = round(bms0$alpha, 3), # Dirichlet parameters
  frequencies = round(bms0$r, 3), # model frequencies
  xp = round(bms0$xp, 3), # exceedance probabilities
  bayes_omni_risk = bms0$bor, #round(bms0$bor, 3) # Bayesian omnibus risk (i.e. probability that model differences are due to chance)
  protected_xp = round(bms0$pxp, 3) # protected exceedance probabilities
)
XP
XP = XP %>%
  mutate(model = factor(model, levels = model_names))

# Plot participant and best fit model curves ----

# a) learning curve ----
participants_learning = DATA %>%
  group_by(trial_number_block, subject) %>% 
  summarise(correct = mean(correct), .groups = 'drop_last') %>%
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3)) %>%
  mutate(model = 'participants')

# sample model choices according to accept probabilities 
# (during fitting, model choice history is the same as participant choices)
# MODELS$model_choice_2 = rbinom(length(MODELS$p_accept), 1, MODELS$p_accept)

models_LP_learning_curves = MODELS %>% 
  mutate(model_choice_2 = (p_accept>0.5)*1,
         model_correct_2 = (model_choice_2 == correct_choice)*1) %>%
  group_by(model, trial_number_block, subject) %>%
  summarise(correct = mean(model_correct_2), .groups = 'drop_last') %>%
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3)) %>%
  rbind(participants_learning) %>%
  filter(model %in% c('1LR', '2LR_F', '2LR_C', '4LR', 'participants')) %>%
  ggplot(aes(x = trial_number_block, y = p_correct, col = model)) + 
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance) +
  geom_ribbon(aes(ymin=p_correct-p_correct_se, ymax=p_correct+p_correct_se, group = model), col = NA, alpha=alpha_ribbon)+
  geom_line() +
  labs(x = 'trial', y = 'proportion correct') +
  coord_cartesian(ylim=lim_accuracy, xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_color_manual(values = models_palette) +
  theme(legend.position = c(0.5,0.8), legend.box = "horizontal") +
  guides(color="none")
models_LP_learning_curves
ggsave(filename = paste(save_path, 'models_LP_learning_curves.svg', sep = .Platform$file.sep), plot = models_LP_learning_curves, 
       width = save_width_models, height = save_height_models, dpi = dpi, units = units, device = 'svg') 


# b) accept curve ----
participants_accept = DATA %>%
  group_by(trial_number_block, subject) %>% 
  summarise(choice = mean(choice), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  mutate(model = 'participants')

models_LP_accept_curves = MODELS %>%
  group_by(model, trial_number_block, subject) %>%
  summarise(choice = mean(p_accept), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  rbind(participants_accept) %>%
  filter(model %in% c('1LR', '2LR_F', '2LR_C', '4LR', 'participants')) %>%
  ggplot(aes(x = trial_number_block, y = p_accept, col = model)) + 
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance) +
  geom_ribbon(aes(group = model, ymin=p_accept-p_accept_se, ymax=p_accept+p_accept_se), col = NA, alpha=alpha_ribbon)+
  geom_line(aes(col = model), show.legend = T) +
  labs(x = 'trial', y = 'proportion accept', lty='') +
  coord_cartesian(ylim=lim_accuracy, xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_color_manual(values = c(models_palette)) +
  theme(legend.position = 'top') +
  guides(color="none")
models_LP_accept_curves
ggsave(filename = paste(save_path, 'models_LP_accept_curves.svg', sep = .Platform$file.sep), plot = models_LP_accept_curves, 
       width = save_width_models, height = save_height_models, dpi = dpi, units = units, device = 'svg') 


# c) sensitivity curves learning phase ----
participants_sensitivity = DATA %>%
  mutate(miniblock = ceiling(trial_number_block/15)) %>%
  group_by(miniblock, rescaled_reward, subject) %>% 
  summarise(choice = mean(choice), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  mutate(model = 'participants')

models_LP_sensitivity_curves = MODELS %>%
  mutate(miniblock = ceiling(trial_number_block/15)) %>%
  group_by(model, miniblock, rescaled_reward, subject) %>%
  summarise(choice = mean(p_accept), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  rbind(participants_sensitivity) %>%
  filter(miniblock %in% c(1, 4))%>%
  filter(model %in% c('1LR', '2LR_F', '2LR_C', '4LR', 'participants')) %>%
  ggplot(aes(x = rescaled_reward, y = p_accept, alpha = miniblock, col = model)) +
  geom_hline(yintercept = 0.5, alpha = alpha_chance, lty = ltp_chance) +
  geom_vline(xintercept = 50, alpha = alpha_chance, lty = ltp_chance) + 
  geom_ribbon(aes(group = interaction(model, miniblock), ymin = p_accept-p_accept_se, ymax = p_accept+p_accept_se), col = NA, alpha=alpha_ribbon) +
  geom_line(aes(col = model, group = interaction(model, miniblock))) +
  labs(x = 'stimulus reward', y = 'proportion accept', lty='', alpha = 'trials') +
  coord_cartesian(ylim=lim_accept, xlim = lim_stim_reward, expand = F) +
  scale_x_continuous(breaks=breaks_stim_reward) +
  scale_alpha_continuous(breaks = c(1,4), labels = c('1 - 15', '46 - 60')) +
  scale_color_manual(values = models_palette) +
  theme(plot.margin = margin(0.5,0.5,0.1,0.1, "cm"),
        panel.spacing.x = unit(0.65, "cm"),
        legend.position = c (0.8,0.3)) +
  guides(color = F, alpha = F)
models_LP_sensitivity_curves
ggsave(filename = paste(save_path, 'models_LP_sensitivity_curves.svg', sep = .Platform$file.sep), plot = models_LP_sensitivity_curves, 
       width = save_width_models, height = save_height_models, dpi = dpi, units = units, device = 'svg') 


# Plot exceedance probabilities and frequencies ----
XP_plot = XP %>%
  ggplot(aes(x = model, y = protected_xp, fill = model)) +
  geom_col(show.legend = F) +
  geom_hline(aes(yintercept = 1/nmodels, size = 'chance'), alpha=alpha_chance, linetype = ltp_chance, show.legend = T) +
  geom_point(aes(y = frequencies, size = 'frequencies', col = model), stroke = 1.5, shape = 23, show.legend = T) +
  guides(fill = F, col = F) +
  scale_fill_manual(values=models_palette) +
  scale_color_manual(values=c(`4LR` = 'white', `2LR_C` = 'black', `2LR_F` = 'black', `1LR` = 'black', 'Rd. Key' = 'black', 'Rd. Choice' = 'black', 'Bandit' = 'black', 'WSLS' = 'black', `1LR_C` ='black',  `1LR_SD` ='black' )) +
  scale_size_manual(name = NULL,
                    values = c(2, .5),
                    breaks = c('frequencies', 'chance'),
                    guide = guide_legend(override.aes = list(linetype = c(0, 4),
                                                             size = c(3, 3),
                                                             shape = c(23, NA), 
                                                             col = c('black', 'black'),
                                                             alpha = c(1,1)) ) ) +
  # scale_color_manual(values=c('chance' = 'black')) +
  labs(y = 'prot. exceedance probabilities', x = 'model') +
  scale_y_continuous(expand = c(0, 0), limits = lim_XP, labels = labels_XP, breaks = breaks_XP) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = c(0.8,0.9)) #, plot.margin = margin(1,0.1,0.1,0.1, "line")) 
XP_plot
ggsave(filename = paste(save_path, 'models_AICc_PXP_all.svg',sep=.Platform$file.sep), plot = XP_plot, width = 6, height = 6, dpi = dpi, units = units)

# Plot number of participants best fit by each model ----
best_fit = model_comparison %>%
  distinct(subject, .keep_all = T) %>%
  group_by(best_model) %>%
  summarise(n = n())
no_best_fit = data.frame(
  best_model = unique(MODELS$model)[!unique(MODELS$model) %in% best_fit$best_model],
  n = 0)
best_fit = rbind(best_fit, no_best_fit) %>%
  left_join(XP %>% select(model, protected_xp, frequencies) %>% rename(best_model = model)) %>%
  left_join(model_comparison %>%
              group_by(model) %>% summarise(AICc = mean(AICc)) %>% rename(best_model = model))

best_fit  %>%
  # mutate(best_model = factor(best_model, levels = c('1LR', '2LR_F', '2LR_C', '4LR', 'basic', 'RC', 'RK'))) %>%
  ggplot(aes(x = best_model, y = n, fill = best_model)) +
  geom_bar(stat = 'identity', show.legend = F) + 
  geom_text(aes(label = n), vjust = -0.2, colour = "black") +
  scale_fill_manual(values=models_palette) +
  labs(y = 'N participants', x = 'best model') + #subtitle = 'labels = exceedance probs'
  # scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        plot.margin = margin(1,0.1,0.1,0.1, "line")) +
  coord_cartesian(clip = 'off')

# Summary stats model comparison ----
best_fit

# Plot simplex of AICc weights ----
# recalculate AICc weights, so they sum to 1
simplex_data = model_comparison %>%
  filter(model %in% c('1LR', '4LR', '2LR_C')) %>%
  select(model, subject, AICc, best_model) %>%
  group_by(subject) %>%
  mutate(AICc_delta = AICc - AICc[model == baseline_model],
         AICc_weight = exp(-.5*AICc_delta) / sum(exp(-.5*AICc_delta)),
         best_model_new = model[AICc == min(AICc)]) %>%
  select(model, AICc_weight, best_model, best_model_new) %>%
  pivot_wider(names_from = model, values_from = AICc_weight) 

simplex = simplex_data %>%
  ggtern(aes(`1LR`, `2LR_C`, `4LR`, col = best_model_new)) +
  geom_mask() +
  geom_point(shape=19,size=2, alpha = 0.5) + 
  theme_classic() +
  theme_showgrid_major() +
  theme_showarrows() +
  theme_clockwise() +
  labs(col = 'best model',
       x       = "1LR",
       xarrow  = "AICc weight 1LR",
       y       = "2LR_C",
       yarrow  = "AICc weight 2LR_C",
       z       = "4LR",
       zarrow  = "AICc weight 4LR") + 
  # scale_fill_manual(values = models_palette) +
  scale_color_manual(values = models_palette) +
  # scale_fill_manual(values = models_palette_b) +
  # scale_color_manual(values = models_palette_b) +
  theme(tern.axis.arrow.start=0.2, tern.axis.arrow.finish=0.8,
        axis.ticks = element_line(c(.2, .4, .6, .8, 1)), text = element_text(size = 12)) +
  scale_T_continuous(breaks=c(.2, .4, .6, .8, 1),labels=c('.2', '.4', '.6', '.8', '1.0'))+
  scale_L_continuous(breaks=c(.2, .4, .6, .8, 1),labels=c('.2', '.4', '.6', '.8', '1.0'))+
  scale_R_continuous(breaks=c(.2, .4, .6, .8, 1),labels=c('.2', '.4', '.6', '.8', '1.0'))
simplex
ggsave(filename = paste(save_path, 'models_AICc_simplex.svg',sep=.Platform$file.sep), plot = simplex, width = 12, height = 6, dpi = dpi, units = units)


# Table of best fit parameters ----
param_table = MODELS %>%
  select(model, subject, c, kappa, choicesd, bias_accept, bias_right, starts_with('initalpha')) %>%
  distinct(.keep_all = T) %>%
  pivot_longer(-c(subject, model), names_to = 'parameter') %>%
  filter(!is.na(value)) %>%
  # mutate(model = factor(model, levels = c('1LR', '2LR_F', '2LR_C', '4LR', 'Bandit', 'RC', 'RK'))) %>%
  group_by(model, parameter) %>%
  summarise(mean = mean(value), 
            sd = sd(value)) %>%
  pivot_wider(names_from = parameter, values_from = c(mean, sd))

param_table[,2:ncol(param_table)] = round(param_table[,2:ncol(param_table)], 2)
param_table
xtable(param_table[,c(1,  2,  3,  5,  4,  7, 11,  6,  9, 10,  8)], type = "latex") #mean
xtable(param_table[,c(1 ,14, 15, 17, 16, 19, 18, 23, 21, 22, 20)], type = "latex") #sd



# 3. Cumulative reward of winning model (4LR) in slow vs. fast condition ---- 
learning_reward = MODELS %>%
  filter(model == '4LR') %>%
  filter(!part_choice == -1) %>%
  group_by(subject, prior, block) %>%
  mutate(received_reward_cumsum = cumsum(model_received_reward) - cumsum(rep(50, length(model_received_reward)))) %>%
  summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
  summarise(received_reward_cumsum = mean(received_reward_cumsum))

reward_boxplot_4LR = learning_reward %>%
  ggplot(aes(x = prior, y = received_reward_cumsum, fill = prior)) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_point(alpha = alpha_individs, col = 'grey', show.legend = F, size = size_point_indivis) + 
  geom_line(aes(group = subject), alpha = alpha_individs,col = 'grey', show.legend = F, size = size_line_indivis) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F, outlier.colour = NA) + 
  labs(x = prior_label, y= 'cum. reward - chance') +
  scale_fill_manual(values = prior_palette) +
  theme(legend.position = c(0.2, .8)) +
  scale_y_continuous(limits = lim_cum_reward, breaks = c(0, 200, 400, 600))
reward_boxplot_4LR
ggsave(filename = paste(save_path, '4LR_reward_boxplot.svg', sep=.Platform$file.sep), plot = reward_boxplot_4LR, 
       width = 5, height = 6.5, dpi = dpi, units = units)


learning_reward_ttest = learning_reward %>%
  group_by(subject) %>%
  summarise(reward_diff = received_reward_cumsum[prior == 'slow'] - received_reward_cumsum[prior == 'fast'])

t.test(x = learning_reward_ttest$reward_diff, g = learning_reward_ttest$subject, alternative = 'greater')

learning_reward %>%
  group_by(prior) %>%
  summarise(mean = mean(received_reward_cumsum),
            se = se(received_reward_cumsum))
mean(learning_reward_ttest$reward_diff)/sd(learning_reward_ttest$reward_diff) # cohen's d

# 4. Correlation relative fit of models vs. 1LR model and slowness effect ----
fit_behav_cor = model_comparison %>%
  left_join(
    DATA %>%
      filter(!choice == -1) %>%
      group_by(subject, prior, block) %>%
      mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
      summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
      summarise(received_reward_cumsum = mean(received_reward_cumsum))%>%
      summarise(reward_diff = received_reward_cumsum[prior == 'slow'] - received_reward_cumsum[prior == 'fast'])
  ) %>%
  left_join(
    DATA_TEST %>%
      group_by(subject, prior) %>%
      summarise(correct = mean(correct), .groups = 'drop_last') %>%
      summarise(correct_diff = correct[prior == 'slow'] - correct[prior == 'fast'])
  )

# plot the correlation model fit and learning phase
fit_reward_corr = fit_behav_cor %>%
  filter(model == '4LR') %>%
  ggplot(aes(x = -AICc_delta, y = reward_diff, col = model)) + 
  geom_hline(yintercept=0, alpha = alpha_chance, linetype = ltp_chance) +
  geom_smooth(method = 'lm', show.legend = F, col = 'black', alpha = alpha_ribbon) +
  geom_point(show.legend = F,  col = 'grey', alpha = alpha_individs, shape=1) + 
  labs(y = 'slow - fast \nlearning phase', x = '1LR - 4LR AICc') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fit_reward_corr
ggsave(filename = paste(save_path, '4LR_fit_reward_corr.svg', sep=.Platform$file.sep), plot = fit_reward_corr, 
       width = 6, height = 3.5, dpi = dpi, units = units)

# calculate the correlations
fit_behav_cor$AICc_delta_neg = - fit_behav_cor$AICc_delta
cor_2LR_F = cor.test(~ AICc_delta_neg + reward_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '2LR_F')
cor_2LR_C = cor.test(~ AICc_delta_neg + reward_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '2LR_C')
cor_4LR = cor.test(~ AICc_delta_neg + reward_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '4LR')
c(cor_2LR_F$p.value, cor_2LR_C$p.value, cor_4LR$p.value)
# p.adjust(c(cor_2LR_F$p.value, cor_2LR_C$p.value, cor_4LR$p.value), method = 'holm')
c(cor_2LR_F$estimate, cor_2LR_C$estimate, cor_4LR$estimate)


# plot the correlation model fit and test phase
fit_accuracy_corr = fit_behav_cor %>%
  filter(model == '4LR') %>%
  ggplot(aes(x = -AICc_delta, y = correct_diff, col = model)) + 
  geom_hline(yintercept=0, alpha = alpha_chance, linetype = ltp_chance) +
  geom_smooth(method = 'lm', show.legend = F, col = 'black', alpha = alpha_ribbon) +
  geom_point(show.legend = F,  col = 'grey', alpha = alpha_individs, shape =1) + 
  labs(y = 'slow - fast \ntest phase', x = '1LR - 4LR AICc') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fit_accuracy_corr
ggsave(filename = paste(save_path, '4LR_fit_accuracy_corr.svg', sep=.Platform$file.sep), plot = fit_accuracy_corr, 
       width = 6, height = 3.5, dpi = dpi, units = units)

# calculate the correlations
fit_behav_cor$AICc_delta_neg = - fit_behav_cor$AICc_delta
cor_2LR_F = cor.test(~ AICc_delta_neg + correct_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '2LR_F')
cor_2LR_C = cor.test(~ AICc_delta_neg + correct_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '2LR_C')
cor_4LR = cor.test(~ AICc_delta_neg + correct_diff, data = fit_behav_cor, subset = fit_behav_cor$model == '4LR')
c(cor_2LR_F$p.value, cor_2LR_C$p.value, cor_4LR$p.value)
# p.adjust(c(cor_2LR_F$p.value, cor_2LR_C$p.value, cor_4LR$p.value), method = 'holm')
c(cor_2LR_F$estimate, cor_2LR_C$estimate, cor_4LR$estimate)


# 5. 4LR best fit learning rates ----
meanLR = MODELS %>%
  filter(model == '4LR') %>%
  group_by(subject, prior, block) %>%
  summarise(alpha_slow_rel = mean(alpha_slow_rel), 
            alpha_slow_irrel = mean(alpha_slow_irrel), 
            alpha_fast_rel = mean(alpha_fast_rel), 
            alpha_fast_irrel = mean(alpha_fast_irrel)) %>%
  summarise(alpha_slow_rel = mean(alpha_slow_rel), 
            alpha_slow_irrel = mean(alpha_slow_irrel), 
            alpha_fast_rel = mean(alpha_fast_rel), 
            alpha_fast_irrel = mean(alpha_fast_irrel)) %>%
  mutate(alpha_slow = ifelse(prior == 'slow', alpha_slow_rel, alpha_slow_irrel),
         alpha_fast = ifelse(prior == 'slow', alpha_fast_irrel, alpha_fast_rel)) %>%
  select(-alpha_slow_rel, -alpha_slow_irrel, -alpha_fast_irrel, -alpha_fast_rel) %>%
  pivot_longer(c(alpha_slow, alpha_fast), names_prefix = 'alpha_', values_to = 'alpha', names_to = 'alpha_feature') %>%
  mutate(relevant = ifelse(prior == alpha_feature, 'relevant', 'irrelevant')) 

meanLRs_4LR = meanLR %>%
  mutate(alpha_feature = factor(alpha_feature, levels = c('slow', 'fast')),
         relevant = factor(relevant, levels = c('relevant', 'irrelevant'))) %>%
  ggplot(aes(x = alpha_feature, y = alpha)) +
  facet_wrap(~relevant) +
  geom_point(width = 0.1, height = 0, alpha = alpha_individs, col = 'grey', size = size_point_indivis) + 
  geom_line(aes(group = subject),  alpha = alpha_individs, col = 'grey', size = size_line_indivis) + 
  geom_boxplot(aes(fill = alpha_feature), alpha = alpha_boxplot, show.legend = F, width = width_boxplot, outlier.colour = NA) +
  # geom_jitter(data = .%>% filter(best_model_4LR), width = 0.2, height = 0, alpha = alpha_individs, col = 'black') + 
  # geom_line(data = .%>% filter(best_model_4LR), aes(group = subject),  alpha = alpha_individs, col = 'black') +
  labs(y = 'mean learning rate', x = 'feature') + #title ='Learning rates (fit)', 
  scale_fill_manual(values = learning_rates_palette) +
  scale_y_continuous(labels = c('0', '.25', '.5', '.75', '1'), breaks = c(0,.25, .50,.75, 1), limits = c(0,1)) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
meanLRs_4LR
ggsave(filename = paste(save_path, '4LR_mean_lrs.svg', sep=.Platform$file.sep), plot = meanLRs_4LR, 
       width = 5, height = 7, dpi = dpi, units = units)

# t-tests slow vs. fast learning rate for relevant and irrelevant separately
meanLR %>%
  group_by(relevant, alpha_feature) %>%
  summarise(mean(alpha), 
            se(alpha))

aov_4LRs = aov(alpha~alpha_feature*relevant+Error(subject/(alpha_feature*relevant)), data = meanLR)
summary(aov_4LRs)

test_4LRs = meanLR %>%
  group_by(subject, relevant) %>%
  summarise(alpha_diff = alpha[alpha_feature == 'slow'] - alpha[alpha_feature == 'fast']) %>%
  mutate(relevant = relevant == 'relevant')
# relevant
LR_rel_ttest = t.test(x = test_4LRs$alpha_diff[test_4LRs$relevant], g = test_4LRs$subject[test_4LRs$relevant])
LR_rel_ttest
mean(test_4LRs$alpha_diff[test_4LRs$relevant])/sd(test_4LRs$alpha_diff[test_4LRs$relevant]) # cohen's d
# irrelevant
LR_irrel_ttest = t.test(x = test_4LRs$alpha_diff[!test_4LRs$relevant], g = test_4LRs$subject[!test_4LRs$relevant])
LR_irrel_ttest
mean(test_4LRs$alpha_diff[!test_4LRs$relevant])/sd(test_4LRs$alpha_diff[!test_4LRs$relevant]) # cohen's d


# 6. correlation between learning rates and behavioural effect -----
# add participant behaviour 
LR_behav_cor = meanLR %>% 
  left_join(
    DATA %>%
      filter(!choice == -1) %>%
      group_by(subject, prior, block) %>%
      mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
      summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
      summarise(received_reward_cumsum = mean(received_reward_cumsum))%>%
      summarise(reward_diff = received_reward_cumsum[prior == 'slow'] - received_reward_cumsum[prior == 'fast'])
    ) %>%
  left_join(
    DATA_TEST %>%
      group_by(subject, prior) %>%
      summarise(correct = mean(correct), .groups = 'drop_last') %>%
      summarise(correct_diff = correct[prior == 'slow'] - correct[prior == 'fast'])
  ) %>%
  mutate(alpha_feature = factor(alpha_feature, levels = c('slow', 'fast')))

# correlation sum of 4LR learning rates and correlation to learning phase reward difference
# plot all 
LR_behav_cor %>%
  ggplot(aes(x = alpha, y = reward_diff)) +
  facet_grid(relevant~alpha_feature, labeller = label_both) +
  geom_smooth(method = 'lm')+
  geom_point()

# test correlations
cor_SR = cor.test(~alpha + reward_diff, data = LR_behav_cor, subset = LR_behav_cor$prior == 'slow' & LR_behav_cor$alpha_feature == 'slow') # slow_rel
cor_SI = cor.test(~alpha + reward_diff, data = LR_behav_cor, subset = LR_behav_cor$prior == 'slow' & LR_behav_cor$alpha_feature == 'fast') # slow_irrel
cor_FR = cor.test(~alpha + reward_diff, data = LR_behav_cor, subset = LR_behav_cor$prior == 'fast' & LR_behav_cor$alpha_feature == 'fast') # fast_rel
cor_FI = cor.test(~alpha + reward_diff, data = LR_behav_cor, subset = LR_behav_cor$prior == 'fast' & LR_behav_cor$alpha_feature == 'slow') # fast_irrel
# c('S,R' = cor_SR$p.value, 'S,I' = cor_SI$p.value, 'F,R' = cor_FR$p.value, 'F,I' = cor_FI$p.value)
p.adjust(c('S,R' = cor_SR$p.value, 'S,I' = cor_SI$p.value, 'F,R' = cor_FR$p.value, 'F,I' = cor_FI$p.value), method = 'holm')
c(cor_SR$estimate, cor_SI$estimate, cor_FR$estimate, cor_FI$estimate)

# plot the relevant learning rate correlations
LR_behav_cor_plot = LR_behav_cor %>%
  filter(relevant == 'relevant') %>%
  ggplot(aes(x = alpha, y = reward_diff, col = alpha_feature)) +
  facet_wrap(~alpha_feature, ncol = 1) +
  geom_hline(yintercept=0, alpha = alpha_chance, linetype = ltp_chance) +
  geom_smooth(method = 'lm', alpha = alpha_ribbon, show.legend = F)+ #col =  learning_rates_palette['slow_rel']
  geom_point(alpha = alpha_individs, show.legend = F, size = size_point_indivis) + #col =  learning_rates_palette['slow_rel'], 
  xlim(0,1) +
  scale_color_manual(values = prior_palette_dark) +
  scale_x_continuous(labels = c('0', '.25', '.5', '.75', '1'), breaks = c(0,.25, .50,.75, 1)) +
  labs(x = 'mean relevant LR', y = 'slow - fast \nlearning phase') +
  theme(strip.background = element_blank())
LR_behav_cor_plot
ggsave(filename = paste(save_path, '4LR_lrs_reward_corr.svg', sep=.Platform$file.sep), plot = LR_behav_cor_plot, 
       width = 6, height = 7, dpi = dpi, units = units)








# ADDITIONAL plots including all models/participants -----

# individual participant and best fit model curves learning curves ----
participants_learning = DATA %>%
  group_by(subject, trial_number_block) %>% 
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3)) %>%
  mutate(model = 'participants')

models_LP_ind = MODELS %>% 
  mutate(model_choice_2 = (p_accept>0.5)*1,
         model_correct_2 = (model_choice_2 == correct_choice)*1) %>%
  group_by(model, subject, trial_number_block) %>%
  summarise(p_correct = mean(model_correct_2), 
            p_correct_se = se(model_correct_2)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3)) %>%
  rbind(participants_learning)
  # filter(model %in% c('1LR', '2LR_F', '2LR_C', '4LR', 'participants')) %>%
  # filter(model %in% c('WSLS', 'Bandit', 'Rd. Choice', 'Rd. Key', '1LR_SD','1LR_C', 'participants')) %>%
  
n_col = ifelse(experiment == 'pilot5', 5, 6)

models_LP_learning_curves_ind = models_LP_ind %>%
  ggplot(aes(x = trial_number_block, y = p_correct, col = model, alpha = model)) + 
  facet_wrap(~subject, ncol = n_col) +
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance) +
  # geom_ribbon(aes(ymin=p_correct-p_correct_se, ymax=p_correct+p_correct_se, group = model), col = NA, alpha=alpha_ribbon)+
  geom_line() +
  labs(x = 'trial', y = 'proportion correct') +
  coord_cartesian(ylim= c(0,1), xlim = lim_trials, expand = F) +
  scale_y_continuous(breaks=breaks_accuracy, labels = c('0', '.5', '1')) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_color_manual(values = models_palette) +
  scale_alpha_manual(values = alpha_palette) +
  theme(legend.position = "bottom", legend.box = "horizontal",strip.text = element_blank()) +
  guides(color = "none",alpha = "none")
models_LP_learning_curves_ind
ggsave(filename = paste(save_path, 'models_LP_learning_curves_ind_control.svg', sep = .Platform$file.sep), plot = models_LP_learning_curves_ind, 
       width = 16, height = 25, dpi = dpi, units = units, device = 'svg') 

# Cumulative reward of all models in slow vs. fast condition ----
reward_boxplot_all = MODELS %>%
  filter(!part_choice == -1) %>%
  group_by(model, subject, prior, block) %>%
  mutate(received_reward_cumsum = cumsum(model_received_reward) - cumsum(rep(50, length(model_received_reward)))) %>%
  summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
  summarise(received_reward_cumsum = mean(received_reward_cumsum)) %>%
  ggplot(aes(x = prior, y = received_reward_cumsum, fill = prior)) +
  facet_wrap(~model, nrow = 1) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_point(alpha = alpha_individs, col = 'grey', show.legend = F, size = size_point_indivis) + 
  geom_line(aes(group = subject), alpha = alpha_individs,col = 'grey', show.legend = F, size = size_line_indivis) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F, outlier.colour = NA) + 
  labs(x = prior_label, y= 'cum. reward - chance') +
  scale_fill_manual(values = prior_palette) +
  theme(legend.position = c(0.2, .8), 
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(limits = lim_cum_reward, breaks = c(0, 200, 400, 600))
reward_boxplot_all
ggsave(filename = paste(save_path, 'models_reward_boxplot.svg', sep=.Platform$file.sep), plot = reward_boxplot_all, 
       width = 10, height = 6.5, dpi = dpi, units = units)

# Cumulative reward of all optimal models in slow vs. fast condition ----
reward_boxplot_all_opt = MODELS_OPT %>%
  filter(!part_choice == -1) %>%
  group_by(model, subject, prior, block) %>%
  mutate(received_reward_cumsum = cumsum(model_received_reward) - cumsum(rep(50, length(model_received_reward)))) %>%
  summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
  summarise(received_reward_cumsum = mean(received_reward_cumsum)) %>%
  ggplot(aes(x = prior, y = received_reward_cumsum, fill = prior)) +
  facet_wrap(~model, nrow = 1) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_point(alpha = alpha_individs, col = 'grey', show.legend = F, size = size_point_indivis) + 
  geom_line(aes(group = subject), alpha = alpha_individs,col = 'grey', show.legend = F, size = size_line_indivis) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F, outlier.colour = NA) + 
  labs(x = prior_label, y= 'cum. reward - chance') +
  scale_fill_manual(values = prior_palette) +
  theme(legend.position = c(0.2, .8), 
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(limits = lim_cum_reward, breaks = c(0, 200, 400, 600))
reward_boxplot_all_opt

ggsave(filename = paste(save_path, 'models_optimal_reward_boxplot.svg', sep=.Platform$file.sep), plot = reward_boxplot_all_opt, 
       width = 10, height = 6.5, dpi = dpi, units = units)

# development of learning rates 4LR model ----
LR_development = MODELS %>%
  filter(model == '4LR') %>%
  select(subject, prior, block, trial_number_block, starts_with('alpha')) %>%
  pivot_longer(-c(subject, prior, block, trial_number_block), names_to = 'parameter', names_prefix = 'alpha_') %>%
  filter(!is.na(value)) %>%
  mutate(parameter = ifelse(parameter == 'slow_rel', 'S,R', parameter),
         parameter = ifelse(parameter == 'slow_irrel', 'S,I', parameter),
         parameter = ifelse(parameter == 'fast_rel', 'F,R', parameter),
         parameter = ifelse(parameter == 'fast_irrel', 'F,I', parameter)) %>%
  mutate(relevance = ifelse((parameter == 'S,R') |(parameter == 'F,R'), 'relevant', 'irrelevant'),
         slowness = ifelse((parameter == 'S,R') |(parameter == 'S,I'), 'slow', 'fast')) %>%
  group_by(parameter, relevance, slowness, trial_number_block, subject) %>%
  summarise(alpha = mean(value)) %>%
  ggplot(aes(x = trial_number_block, y = alpha, group = subject, col = parameter)) +
  facet_grid(relevance~slowness) +
  geom_line(show.legend = F, alpha = .5) +
  scale_color_manual(values = learning_rates_palette) +
  scale_y_continuous(labels = c('0', '.5', '1'), breaks = c(0, .50, 1), limits = c(0,1)) +
  theme(strip.background = element_blank()) +
  labs(x = 'trial', y = 'learning rate')
LR_development

ggsave(filename = paste(save_path, '4LR_LR_development.svg', sep=.Platform$file.sep), plot = LR_development, 
       width = 5, height = 6, dpi = dpi, units = units)
