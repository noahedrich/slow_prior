## GGPLOT SETUP
# Settings and variables for use in plotting
# So that the colours/sizes/transparency etc. is consistent 
library(ggplot2)
library(svglite)
library(ggExtra)
library(viridis)

# GGPLOT THEME ----
theme_set(theme_classic(base_size = 12))

# COLOUR PALETTES ----
# model colours from viridis: magma and mako
prior_palette = c(slow = '#BEAED4', fast = '#7FC97F')
prior_palette_dark = c(slow = '#7E6DAB', fast = '#2E6946')
models_palette = c(`4LR` = '#782281FF', `2LR_C` = '#BF3A77FF', `2LR_F` = '#F66D5CFF', `1LR` = '#FEC185FF', 
                   'Rd. Key' = '#A8E1BCFF', 'Rd. Choice' = '#3DB4ADFF', 'rd. key' = '#A8E1BCFF', 'rd. choice' = '#3DB4ADFF',  'Bandit' = '#357BA2FF', 'WSLS' = '#414081FF',
                   '1LR_C' = '#A49B76FF', '1LR_SD' = '#C5B56CFF',
                   'participants' = 'black')

learning_rates_palette = c(initalpha = 'black', 
                           initalpha_slow = '#BEAED4', slow = '#BEAED4', initalpha_slow_irrel = '#BEAED4', slow_irrel = '#BEAED4',`S,I` = '#BEAED4',
                           initalpha_fast = '#7FC97F', fast = '#7FC97F', initalpha_fast_irrel = '#7FC97F', fast_irrel = '#7FC97F',`F,I` = '#7FC97F',
                           initalpha_slow_rel = '#7E6DAB', slow_rel = '#7E6DAB', `S,R` = '#7E6DAB',
                           initalpha_fast_rel = '#2E6946', fast_rel = '#2E6946', `F,R` = '#2E6946')

alpha_model = 0.6
alpha_palette = c(`4LR` = alpha_model, `2LR_C` = alpha_model, `2LR_F` = alpha_model, `1LR` = alpha_model, 
                  'Rd. Key' = alpha_model, 'Rd. Choice' = alpha_model, 'rd. key' = alpha_model, 'rd. choice' = alpha_model,  'Bandit' = alpha_model, 'WSLS' = alpha_model,
                  '1LR_C' = alpha_model, '1LR_SD' = alpha_model, '1LR_fix' = alpha_model,
                  'participants' = 1)

# PLOT SAVING SIZE ----
units = 'cm'
dpi = 300

save_width_validation = 10.5
save_height_validation = 5.5

fig_width = 6
fig_height = 6

save_width = 10
save_height = 6

save_width_models = 5
save_height_models = 5

# PLOT ELEMENTS ----
alpha_individs = .8
alpha_boxplot = .7
alpha_chance = .5
alpha_ribbon = .15
ltp_chance = 2
width_boxplot = .5
size_point_indivis = 0.5
size_line_indivis = 0.2

# AXES ----
prior_label = 'condition'
lim_reward = c(2800, 3735)
lim_cum_reward = c(-120, 675) # c(-120, 450) #
lim_cum_reward_lines = c(-50, 300)# c(-50, 200)# 
lim_accuracy = c(0.4, 1)
lim_accept = c(0, 1)
lim_trials = c(1,60)#c(1,45) #
lim_stim_reward = c(0, 100)
lim_XP = c(0,1)
lim_alpha = c(0, 1)
labels_XP = c('0', '.25', '.50', '.75', '1')
breaks_XP = c(0,.25, .5, .75, 1)
breaks_alpha = c(0,.25, .5, .75, 1)
breaks_trials = c(1,seq(0, 60, 15))
breaks_stim_reward = seq(0,100,25)
breaks_accuracy = seq(0,1,.5)
