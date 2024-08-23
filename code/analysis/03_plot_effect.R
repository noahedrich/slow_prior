# All behavioural figures
# MAIN EFFECT PLOTS
# Learning phase:
# 1. cumulative reward boxplot 
# 2. cumulative reward over trials
# Test phase:
# 3. accuracy boxplot 
# S. Learning curves, split by condition

# !! make sure to set WD to the folder of this file before running, so that here() works
# !! here() should point to the 'code' folder

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))

# MAIN EFFECT PLOTS ---- 
# 1. plot cumulative reward boxplot ----
LP_reward_boxplot = DATA %>%
  group_by(experiment, subject, prior, block) %>%
  mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
  summarise(received_reward_cumsum = tail(received_reward_cumsum, 1)) %>%
  summarise(received_reward_cumsum = mean(received_reward_cumsum)) %>%
  ggplot(aes(x = prior, y = received_reward_cumsum, fill = prior)) +
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_point(alpha = alpha_individs, col = 'grey', show.legend = F) + 
  geom_line(aes(group = subject), alpha = alpha_individs,col = 'grey', show.legend = F) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F, outlier.colour = NA) + 
  labs(x = prior_label, y= 'cum. reward - chance') +
  scale_fill_manual(values = prior_palette) +
  theme(legend.position = c(0.2, .8),
        strip.background = element_blank()) +
  scale_y_continuous(limits = lim_cum_reward, breaks = c(0, 200, 400, 600))
LP_reward_boxplot

ggsave(filename = paste(save_path, 'participants_LP_reward_boxplot.svg', sep = .Platform$file.sep), 
       plot = LP_reward_boxplot, width = save_width, height = save_height, dpi = dpi, units = units, device = 'svg') 

# 2. plot cumulative reward over trials ----
LP_reward_line = DATA %>%
  group_by(experiment, subject, block) %>%
  mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
  group_by(experiment, trial_number_block, prior, subject) %>%
  summarise(cumsum = mean(received_reward_cumsum)) %>%
  summarise(mean = mean(cumsum), 
            se = se(cumsum)) %>%
  ggplot(aes(x = trial_number_block, y = mean, col = prior)) +
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se, group = prior), alpha = alpha_ribbon, col = NA) +
  geom_line() +
  labs(x = 'trial', y= 'cum. reward - chance', col = prior_label) +
  coord_cartesian(ylim = c(-40, NA), xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_color_manual(values = prior_palette_dark) +
  theme(legend.position = c(0.2, .8),
        strip.background = element_blank()) +
  ylim(lim_cum_reward_lines)
LP_reward_line

ggsave(filename = paste(save_path, 'participants_LP_reward_line.svg', sep = .Platform$file.sep), plot = LP_reward_line, 
       width = save_width, height = save_height, dpi = dpi, units = units, device = 'svg')

# 2b. plot cumulative reward *difference* over trials ----
LP_reward_line_diff = DATA %>%
  group_by(experiment, subject, block) %>%
  mutate(received_reward_cumsum = cumsum(received_reward) - cumsum(rep(50, length(received_reward)))) %>%
  group_by(experiment, trial_number_block, subject, prior) %>%
  summarise(cumsum = mean(received_reward_cumsum)) %>%
  summarise(cumsum_diff = cumsum[prior == 'slow'] - cumsum[prior == 'fast']) %>%
  summarise(mean = mean(cumsum_diff), 
            se = se(cumsum_diff)) %>%
  ggplot(aes(x = trial_number_block, y = mean)) +
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0, alpha =alpha_chance, lty=ltp_chance)+
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se), alpha = alpha_ribbon, col = NA) +
  geom_line() +
  labs(x = 'trial', y= 'slow - fast cum. reward', col = prior_label) +
  coord_cartesian(xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=c(1,30,60)) +
  theme(legend.position = c(0.2, .8),
        strip.background = element_blank())
LP_reward_line_diff

ggsave(filename = paste(save_path, 'participants_LP_reward_line_diff.svg', sep = .Platform$file.sep), plot = LP_reward_line_diff, 
       width = save_width*0.6, height = save_height, dpi = dpi, units = units, device = 'svg')


# 3. plot test phase accuracy boxplot ----
TP_accuracy_boxplot = DATA_TEST %>% 
  group_by(experiment, subject, prior, block) %>%
  summarise(correct = mean(correct), .groups = 'drop_last') %>%
  summarise(correct = mean(correct)) %>%
  ggplot(aes(x = prior, y = correct)) + 
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance) +
  geom_point(alpha = alpha_individs, col = 'grey') + 
  geom_line(aes(group = subject), alpha = alpha_individs, col = 'grey', show.legend = F) + 
  geom_boxplot(aes(fill = prior), alpha = alpha_boxplot, width = width_boxplot, show.legend = F) + 
  theme(strip.background = element_blank()) +
  labs(x = prior_label, y = 'proportion correct') + 
  scale_y_continuous(limits = lim_accuracy, breaks = seq(lim_accuracy[1], lim_accuracy[2], by = 0.1)) +
  scale_fill_manual(values = prior_palette)
TP_accuracy_boxplot

ggsave(filename = paste(save_path, 'participants_TP_accuracy_boxplot.svg', sep = .Platform$file.sep), plot = TP_accuracy_boxplot, 
       width = save_width, height = save_height, dpi = dpi, units = units, device = 'svg') 

# S. learning curve, split by condition ----
participants_LP_learning_curves_prior = DATA %>%
  group_by(experiment, prior,trial_number_block, subject) %>%  #
  summarise(correct = mean(correct), .groups = 'drop_last') %>%
  summarise(p_correct = mean(correct), 
            p_correct_se = se(correct)) %>%
  mutate(p_correct = rolling_mean(p_correct, n=3),
         p_correct_se = rolling_mean(p_correct_se, n=3)) %>%
  ggplot(aes(x = trial_number_block, y = p_correct, col = prior)) + #
  facet_wrap(~experiment) +
  geom_hline(yintercept = 0.5, alpha=alpha_chance, linetype = ltp_chance, linewidth = 1) +
  geom_line() +
  geom_ribbon(aes(ymin=p_correct-p_correct_se, ymax=p_correct+p_correct_se, group = prior), col = NA, alpha=alpha_ribbon)+ #
  labs(x = 'trial', y = 'proportion correct', col = prior_label) +
  coord_cartesian(ylim=lim_accuracy, xlim = lim_trials, expand = F) +
  scale_x_continuous(breaks=breaks_trials) +
  scale_y_continuous(breaks=c('.4'=.4,'.5'=.5,'.6'=.6,'.7'=.7,'.8'=.8,'.9'=.9,'1'=1)) +
  scale_color_manual(values = prior_palette) +
  theme(legend.position = 'top',
        strip.background = element_blank())
participants_LP_learning_curves_prior
ggsave(filename = paste(save_path, 'participants_LP_learning_curves_condition.svg', sep = .Platform$file.sep), plot = participants_LP_learning_curves_prior, 
       width = save_width_validation, height = 6, dpi = dpi, units = units, device = 'svg') 

