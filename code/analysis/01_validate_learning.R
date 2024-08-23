# Basic behavioural sanity check that participants learned the task 
# 1. Accuracy first vs last 10 trials of learning phase
# - learning curve
# 2. Accepting first vs last 10 trials of learning phase
# - accept curve
# - sensitivity curves learning phase
# 3. Test phase accuracy vs. chance 
# - sensitivity curves test phase 

# !! make sure to set WD to the folder of this file before running, so that here() works
# !! here() should point to the 'code' folder

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))

# 1. LEARNING PHASE: Accuracy first vs last 10 trials ----
learning = DATA %>%
  group_by(experiment) %>%
  mutate(last_10 = max(trial_number_block)-10) %>%
  ungroup() %>%
  mutate(phase = ifelse(trial_number_block < 11, 'start', 
                        ifelse(trial_number_block > last_10, 'end', NA))) %>%
  filter(!is.na(phase)) %>%
  mutate(phase = factor(phase, levels = c('start', 'end'))) %>%
  group_by(experiment, phase, subject) %>%
  summarise(p_accept = mean(choice),
            p_correct = mean(correct))

# descriptives and t-test
learning %>%
  group_by(experiment, phase) %>%
  summarise(p_correct = round(mean(p_correct), 2))

learning_test = learning %>%
  group_by(experiment, subject) %>%
  summarise(diff = p_correct[phase== 'end'] - p_correct[phase== 'start'])

t.test(learning_test$diff[learning_test$experiment== "experiment1"])
t.test(learning_test$diff[learning_test$experiment== "experiment2"])
t.test(learning_test$diff[learning_test$experiment== "replication"])


#  learning curve ----
participants_learning = DATA %>%
  mutate(model = 'participants') %>%
  group_by(model, experiment, trial_number_block, subject) %>% 
  summarise(correct = mean(correct), .groups = 'drop_last') %>%
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3))

models_learning = MODELS %>% 
  filter(model %in% c('Rd. Choice', 'Rd. Key')) %>%
  mutate(model_choice_2 = (p_accept>0.5)*1,
         model_correct_2 = (model_choice_2 == correct_choice)*1) %>%
  group_by(model, experiment, trial_number_block, subject) %>%
  summarise(correct = mean(model_correct_2), .groups = 'drop_last') %>%
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3))

participants_LP_learning_curves = participants_learning %>%
  rbind(models_learning) %>%
  ggplot(aes(x = trial_number_block, y = p_correct, col = model)) +
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance, linewidth = 1) +
  geom_line(show.legend = F) +
  geom_ribbon(aes(ymin=p_correct-p_correct_se, ymax=p_correct+p_correct_se, group = model), col = NA, alpha=alpha_ribbon)+
  labs(x = 'trial', y = 'proportion correct', col = 'model') +
  coord_cartesian(ylim=lim_accuracy, xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_y_continuous(breaks=c('.4'=.4,'.5'=.5,'.6'=.6,'.7'=.7,'.8'=.8,'.9'=.9,'1'=1)) +
  scale_color_manual(values = models_palette) +
  theme(legend.position = c(0.5,0.8),
        legend.direction = 'horizontal',
        strip.background = element_blank())
participants_LP_learning_curves

ggsave(filename = paste(save_path, 'participants_LP_learning_curves.svg', sep = .Platform$file.sep), plot = participants_LP_learning_curves, 
       width = save_width_validation, height = save_height_validation, dpi = dpi, units = units, device = 'svg') 

# 2. LEARNING PHASE: Accepting first vs last 10 trials ----
# descriptives and t-test
learning %>%
  group_by(experiment, phase) %>%
  summarise(p_accept = round(mean(p_accept), 2))

accept_test = learning %>%
  group_by(experiment, subject) %>%
  summarise(diff = p_accept[phase== 'end'] - p_accept[phase== 'start'])

t.test(accept_test$diff[accept_test$experiment== "experiment1"])
t.test(accept_test$diff[accept_test$experiment== "experiment2"])
t.test(accept_test$diff[accept_test$experiment== "replication"])


#  accept curve ----
participants_LP_accept_curves = DATA %>%
  group_by(experiment, trial_number_block, subject) %>% 
  summarise(choice = mean(choice), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  ggplot(aes(x = trial_number_block, y = p_accept)) + 
  facet_wrap(~experiment) +
  geom_ribbon(aes(ymin=p_accept-p_accept_se, ymax=p_accept+p_accept_se), col = NA, alpha=alpha_ribbon)+
  geom_line() +
  labs(x = 'trial', y = 'proportion accept', col = prior_label) +
  coord_cartesian(ylim=lim_accuracy, xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_y_continuous(breaks=c('.4'=.4,'.5'=.5,'.6'=.6,'.7'=.7,'.8'=.8,'.9'=.9,'1'=1)) +
  theme(legend.position = 'top',
        strip.background = element_blank())
participants_LP_accept_curves

ggsave(filename = paste(save_path, 'participants_LP_accept_curves.svg', sep = .Platform$file.sep), plot = participants_LP_accept_curves, 
       width = save_width_validation, height = save_height_validation, dpi = dpi, units = units, device = 'svg') 



#  sensitivity curves learning phase ----
participants_LP_sensitivity_curves = DATA %>%
  mutate(miniblock = ceiling(trial_number_block/15)) %>%
  group_by(experiment, miniblock, rescaled_reward, subject) %>% #miniblock, 
  summarise(choice = mean(choice), .groups = 'drop_last') %>%
  summarise(p_accept = mean(choice), 
            p_accept_se = se(choice)) %>%
  mutate(p_accept = rolling_mean(p_accept, n=3),
         p_accept_se = rolling_mean(p_accept_se, n=3)) %>%
  ggplot(aes(x = rescaled_reward, y = p_accept, alpha = miniblock)) +
  facet_wrap(~experiment) +
  geom_line(aes(group = miniblock)) +
  geom_ribbon(aes(ymin = p_accept-p_accept_se, ymax = p_accept+p_accept_se, group = miniblock), col = NA, alpha=alpha_ribbon) +
  geom_hline(yintercept = 0.5, alpha = alpha_chance, lty = ltp_chance) +
  geom_vline(xintercept = 50, alpha = alpha_chance, lty = ltp_chance) + 
  labs(x = 'stimulus reward', y = 'proportion accept', alpha = 'trials') +
  coord_cartesian(ylim=lim_accept, xlim = lim_stim_reward, expand = F) +
  scale_x_continuous(breaks=breaks_stim_reward) +
  scale_alpha_continuous(breaks = 1:4, labels = c('1 - 15', '16 - 30', '31 - 45', '46 - 60')) +
  theme(plot.margin = margin(0.5,0.5,0.1,0.1, "cm"),
        panel.spacing.x = unit(0.65, "cm"),
        legend.position = c (0.8,0.3),
        strip.background = element_blank())
participants_LP_sensitivity_curves

ggsave(filename = paste(save_path, 'participants_LP_sensitivity_curves.svg', sep = .Platform$file.sep), plot = participants_LP_sensitivity_curves, 
       width = save_width_validation, height = save_height_validation, dpi = dpi, units = units, device = 'svg') 


# 3. TEST PHASE: accuracy vs. chance ----
test = DATA_TEST %>%
  group_by(experiment, subject, block) %>%
  summarise(p_correct = mean(correct), .groups = 'drop_last') %>%
  summarise(p_correct = mean(p_correct))

# descriptives and t-test
test %>%
  group_by(experiment) %>%
  summarise(p_correct = round(mean(p_correct),2))

t.test(test$p_correct[test$experiment == "experiment1"], mu = 0.5)
t.test(test$p_correct[test$experiment == "experiment2"], mu = 0.5)
t.test(test$p_correct[test$experiment == "replication"], mu = 0.5)

# sensitivity curves test trials ----
participants_TP_sensitivity_curves = DATA_TEST %>%
  mutate(diff = rescaled_reward_right - rescaled_reward_left,
         choice = ifelse(key_press == 74, 1, 0)) %>%
  group_by(experiment, diff, subject) %>%
  summarise(choice = mean(choice), .groups = 'drop_last') %>%
  summarise(p_right = mean(choice),
            p_right_se = se(choice)) %>%
  mutate(p_right = rolling_mean(p_right, n=5),
         p_right_se = rolling_mean(p_right_se, n=5)) %>%
  ggplot(aes(x = diff, y = p_right)) +
  facet_wrap(~experiment) +
  geom_ribbon(aes(ymin = p_right-p_right_se, ymax = p_right+p_right_se), col = NA, alpha=alpha_ribbon) +
  geom_line() +
  geom_hline(yintercept = 0.5, alpha = alpha_chance, lty = ltp_chance) +
  geom_vline(xintercept = 0, alpha = alpha_chance, lty = ltp_chance) + 
  labs(x = 'right - left reward', y = "proportion right") +
  coord_cartesian(ylim=lim_accept, xlim = c(-50, 50), expand = F) +
  scale_x_continuous(breaks=c(-50,-25,0,25,50)) +
  theme(plot.margin = margin(0.5,0.5,0.1,0.1, "cm"),
        panel.spacing.x = unit(0.65, "cm"),
        legend.position = c (0.8,0.3),
        strip.background = element_blank())
participants_TP_sensitivity_curves

ggsave(filename = paste(save_path, 'participants_TP_sensitivity_curves.svg', sep = .Platform$file.sep), plot = participants_TP_sensitivity_curves, 
       width = save_width_validation, height = save_height_validation, dpi = dpi, units = units, device = 'svg') 
