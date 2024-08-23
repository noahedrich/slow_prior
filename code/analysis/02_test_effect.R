# Behavioural analyses
# Learning phase:
# 1. t-test reward
# 2. t-test accuracy 
# 3. mixed effects choices by reward
# 4. mixed effects relevant vs. irrelevant feature
# Test phase:
# 5. t-test accuracy 
# 6. mixed effects choices by reward difference

# !! make sure to set WD to the folder of this file before running, so that here() works
# !! here() should point to the 'code' folder

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))

# filter out one experiment sample
experiment_name = 'replication'
DATA = DATA %>% filter(experiment == experiment_name)
DATA_TEST = DATA_TEST %>% filter(experiment == experiment_name)


# PREPARE DATA ----
# LEARNING PHASE
# exclude no answer trials (& bad performers?)
DATA$subject = as.factor(DATA$subject)
ids = unique(DATA$subject)
nids = length(ids)


# make some useful variables 
DATA$rescaled_reward_z = DATA$rescaled_reward_cumsum = DATA$received_reward_z = DATA$received_reward_cumsum_z = DATA$trial_number_block_z = DATA$abs_rescaled_reward_z = NA

for (cid in ids) {
  for (cblock in 1:8) {
    cidx = which(DATA$subject == cid & DATA$block == cblock)
    DATA$received_reward_cumsum[cidx] = cumsum(DATA$received_reward[cidx]) - cumsum(rep(50, length(cidx))) # cumulative reward, relative to the chance baseline of collecting 50 on each trial 
  }
  cidx = which(DATA$subject == cid)
  DATA$abs_rescaled_reward_z[cidx] = c(scale(abs(DATA$rescaled_reward[cidx] - 50)))
  DATA$rescaled_reward_z[cidx] = c(scale(DATA$rescaled_reward[cidx]))
  DATA$received_reward_z[cidx] = c(scale(DATA$received_reward[cidx]))
  DATA$received_reward_cumsum_z[cidx] = c(scale(DATA$received_reward_cumsum[cidx]))
}

DATA$trial_number_block_z = log(DATA$trial_number_block)/log(max(DATA$trial_number_block))

# variables for circular regressions
DATA = DATA %>%
  group_by(subject) %>%
  mutate(pos_rel = ifelse(rewarded == 'color', pos_color, pos_shape), #angles
         pos_irrel = ifelse(rewarded == 'color', pos_shape, pos_color),
         pos_rel_recoded = pos_rel + (0 - pos_reward), # recoding so reward at 0 deg. - can replace 0 with any location
         pos_rel_recoded = ifelse(pos_rel_recoded>360, pos_rel_recoded-360, 
                                  ifelse(pos_rel_recoded<0, pos_rel_recoded+360, pos_rel_recoded)),
         rad_rel = pos_rel_recoded*pi/180, #radians
         rad_irrel = pos_irrel*pi/180
  )

summary(DATA)


# TEST PHASE - preprocess data---- 
DATA_TEST$subject = as.factor(DATA_TEST$subject)

DATA_TEST$choice = ifelse(DATA_TEST$key_press == 70, 0, 1) # 0 = left
DATA_TEST$received_reward = ifelse(DATA_TEST$choice == 0, DATA_TEST$rescaled_reward_left, DATA_TEST$rescaled_reward_right)
DATA_TEST$rescaled_reward_max = apply(matrix(c(DATA_TEST$rescaled_reward_left, DATA_TEST$rescaled_reward_right), ncol = 2), 1, max)
DATA_TEST$rescaled_reward_min = apply(matrix(c(DATA_TEST$rescaled_reward_left, DATA_TEST$rescaled_reward_right), ncol = 2), 1, min)

# make some useful variables 
DATA_TEST$rescaled_reward_left_z = DATA_TEST$rescaled_reward_right_z = DATA_TEST$rescaled_reward_diff_z = DATA_TEST$rescaled_reward_diff_abs_z = DATA_TEST$trial_number_block_z = DATA_TEST$rt_log = DATA_TEST$received_reward_z = DATA_TEST$regret_z = DATA_TEST$boundary_dist_z = NA

for (cid in ids) {
  # for (cblock in 1:8) {
  #   cidx = which(DATA$subject == cid & DATA$block == cblock)
  #   DATA$received_reward_cumsum[cidx] = cumsum(DATA$received_reward[cidx]) - cumsum(rep(50, length(cidx))) # cumulative reward, relative to the chnace baseline of collecting 50 on each trial 
  # }
  cidx = which(DATA_TEST$subject == cid)
  DATA_TEST$rescaled_reward_left_z[cidx] = c(scale(DATA_TEST$rescaled_reward_left[cidx]))
  DATA_TEST$rescaled_reward_right_z[cidx] = c(scale(DATA_TEST$rescaled_reward_right[cidx]))
  DATA_TEST$rescaled_reward_diff_z[cidx] = c(scale(DATA_TEST$rescaled_reward_right[cidx] - DATA_TEST$rescaled_reward_left[cidx]))
  DATA_TEST$rescaled_reward_diff_abs_z[cidx] = c(scale(abs(DATA_TEST$rescaled_reward_right[cidx] - DATA_TEST$rescaled_reward_left[cidx])))
  DATA_TEST$received_reward_z[cidx] = c(scale(DATA_TEST$received_reward[cidx]))
  DATA_TEST$rescaled_reward_max_z = c(scale(DATA_TEST$rescaled_reward_max))
  DATA_TEST$rescaled_reward_min_z = c(scale(DATA_TEST$rescaled_reward_min))
  # DATA_TEST$received_reward_cumsum_z[cidx] = c(scale(DATA_TEST$received_reward_cumsum[cidx]))
}

DATA_TEST$trial_number_block_z = log(DATA_TEST$pair_number_block)/log(max(DATA_TEST$pair_number_block))

summary(DATA_TEST)






# settings for LME fitting ----
cglmctrl = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

# Procedure to simplify random effects when they don't converge: 
# 1. remove all interactions
# 2. remove all main effects that are not of interest to the main comparison
# 3. remove all random slopes (keep only intercept)


# 1. LEARNING PHASE: t-test reward ----
# means within each block and normalized by number of trials 
cmat = apply(tapply(DATA$received_reward_cumsum, list(DATA$block, DATA$prior, DATA$subject), function(x) tail(x, 1)), c(2, 3), mean, na.rm = TRUE)
t.test(cmat['slow',] - cmat['fast',], alternative = 'greater')
apply(cmat, 1, mean)
apply(cmat, 1, se)
mean(cmat['slow',]-cmat['fast',])/sd(cmat['slow',]-cmat['fast',])

# 2. LEARNING PHASE: t-test accuracy ----
cmat = tapply(DATA$correct, list(DATA$prior, DATA$subject), mean)
t.test(cmat['slow',] - cmat['fast',], alternative = 'greater')
apply(cmat, 1, mean)
apply(cmat, 1, se)
mean(cmat['slow',]-cmat['fast',])/sd(cmat['slow',]-cmat['fast',])


# 3. LEARNING PHASE: mixed effects, choices by reward ----
# logistic mixed effects model to predict choices based on the stimulus reward on each trial, 
# and examine the interaction with condition (slow/fast). 
choice_glme1 = glmer(choice ~ 
                       trial_number_block_z*rescaled_reward_z + trial_number_block_z*prior + rescaled_reward_z*prior + 
                       (1 + trial_number_block_z + rescaled_reward_z + prior |subject), 
                     family = binomial, data = DATA, control = cglmctrl)

# remove non-significant main effects - Likelihood Ratio Test 
choice_glme1_comparison = drop1(choice_glme1, test = 'Chisq') #, scope = .~.
choice_glme1_comparison

# correct for multiple comparisons
# main effect of condition, interaction condition*reward, interaction condition*trial
choice_glme1_comparison$`Pr(Chi)`
p.adjust(p = choice_glme1_comparison$`Pr(Chi)`, method = 'holm', n = 3) 

# Get the betas and confidence intervals
beta_se = sqrt(diag(vcov(choice_glme1))) # standard error for each coefficient (approx.)?
# table of estimates with 95% CI
betas = cbind(Est = fixef(choice_glme1), LL = fixef(choice_glme1) - 1.96 * beta_se, UL = fixef(choice_glme1) + 1.96 * beta_se)
betas #logit scale
# exp(betas) # log odds
# exp(betas) / (1 + (exp(betas))) # probability

# table for paper
xtable(betas, type = "latex")

# sd and correlation of random effects
VarCorr(choice_glme1)
xtable(data.frame(VarCorr(choice_glme1))[1:4,c(1,2,5)])


# 4. LEARNING PHASE: mixed effects relevant vs. irrelevant feature (CIRCULAR PREDICTORS) ----
# Is the slow feature more predictive of choice, regardless of relevance? 
# logistic mixed effects model to predict choices based on the stimulus colour and shape positions 
# on each trial. We will do so separately for slow and fast blocks. 
# As this analysis is run separately for each condition, no model comparison is done.
DATA_SLOW = subset(DATA, prior == 'slow')
DATA_FAST = subset(DATA, prior == 'fast')
DATA_SLOW$block = as.factor(DATA_SLOW$block)
DATA_FAST$block = as.factor(DATA_FAST$block)

# SLOW --
if (experiment_name == 'experiment1' | experiment_name == 'experiment2') {
  # full model 
  circ_glme1_slow = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                            cos(rad_irrel)*trial_number_block_z + sin(rad_irrel)*trial_number_block_z + 
                            (1 | subject), data = DATA_SLOW, family = binomial, control = cglmctrl) 
  # model without irrelevant feature predictors to compare (keep same rdfx structure)
  circ_glme1_slow_reduced = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                                    (1 | subject), data = DATA_SLOW, family = binomial, control = cglmctrl) 
} else { # replication
  circ_glme1_slow = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                            cos(rad_irrel)*trial_number_block_z + sin(rad_irrel)*trial_number_block_z + 
                            (1 + trial_number_block_z + cos(rad_rel) + cos(rad_irrel) + sin(rad_rel) + sin(rad_irrel)| subject), data = DATA_SLOW, family = binomial, control = cglmctrl) 
  circ_glme1_slow_reduced = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                                    (1 + trial_number_block_z + cos(rad_rel) + cos(rad_irrel) + sin(rad_rel) + sin(rad_irrel)| subject), data = DATA_SLOW, family = binomial, control = cglmctrl) 
}

# does irrelevant feature predict choices? - Likelihood Ratio Test 
circ_glme1_slow_comparison = anova(circ_glme1_slow, circ_glme1_slow_reduced, test='Chisq')
circ_glme1_slow_comparison

# correct for multiple comparisons
# the main effect of the irrelevant feature and its interaction with trial in both the slow and fast blocks
circ_glme1_slow_comparison$`Pr(>Chisq)`
p.adjust(p = circ_glme1_slow_comparison$`Pr(>Chisq)`, method = 'holm', n = 4) 

# standard error for each coefficient (approx.)?
slow_se = sqrt(diag(vcov(circ_glme1_slow)))
# table of estimates with 95% CI
betas_slow = cbind(Est = fixef(circ_glme1_slow), LL = fixef(circ_glme1_slow) - 1.96 * slow_se, UL = fixef(circ_glme1_slow) + 1.96 * slow_se)
betas_slow #logit scale
# exp(betas_slow) # log odds
# exp(betas_slow) / (1 + (exp(betas_slow))) # probability

# table for paper
xtable(betas_slow)
# sd and corr of random effects
VarCorr(circ_glme1_slow)

# FAST --
if (experiment_name == 'experiment1' | experiment_name == 'experiment2') {
  circ_glme1_fast = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                            cos(rad_irrel)*trial_number_block_z + sin(rad_irrel)*trial_number_block_z + 
                            (1 | subject), data = DATA_FAST, family = binomial, control = cglmctrl) 
  circ_glme1_fast_reduced = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                                    (1 | subject), data = DATA_FAST, family = binomial, control = cglmctrl) 
} else { # replication
  circ_glme1_fast = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                            cos(rad_irrel)*trial_number_block_z + sin(rad_irrel)*trial_number_block_z + 
                            (1 + trial_number_block_z + cos(rad_rel) + cos(rad_irrel) + sin(rad_rel) + sin(rad_irrel)| subject), data = DATA_FAST, family = binomial, control = cglmctrl) 
  circ_glme1_fast_reduced = glmer(choice ~ cos(rad_rel)*trial_number_block_z + sin(rad_rel)*trial_number_block_z + 
                                    (1 + trial_number_block_z + cos(rad_rel) + cos(rad_irrel) + sin(rad_rel) + sin(rad_irrel)| subject), data = DATA_FAST, family = binomial, control = cglmctrl) 
}


# does irrelevant feature predict choices? - Likelihood Ratio Test 
circ_glme1_fast_comparison = anova(circ_glme1_fast, circ_glme1_fast_reduced, test='Chisq')
circ_glme1_fast_comparison

# correct for multiple comparisons
# the main effect of the irrelevant feature and its interaction with trial in both the slow and fast blocks
circ_glme1_fast_comparison$`Pr(>Chisq)`
p.adjust(p = circ_glme1_fast_comparison$`Pr(>Chisq)`, method = 'holm', n = 4) 

# standard error for each coefficient (approx.)?
fast_se = sqrt(diag(vcov(circ_glme1_fast)))
# table of estimates with 95% CI
betas_fast = cbind(Est = fixef(circ_glme1_fast), LL = fixef(circ_glme1_fast) - 1.96 * fast_se, UL = fixef(circ_glme1_fast) + 1.96 * fast_se)
betas_fast #logit scale
# exp(betas_fast) # log odds
# exp(betas_fast) / (1 + (exp(betas_fast))) # probability

# table for paper 
xtable(betas_fast)
VarCorr(circ_glme1_fast)


# 5. TEST PHASE: t-test accuracy ----
cmat = tapply(DATA_TEST$correct, list(DATA_TEST$prior, DATA_TEST$subject), mean)
t.test(cmat['slow',] - cmat['fast',], alternative = 'greater')
apply(cmat, 1, mean)
apply(cmat, 1, se)
mean(cmat['slow',] - cmat['fast',])/sd(cmat['slow',] - cmat['fast',])

# 6. TEST PHASE: mixed effects choices by reward difference ----
# logistic mixed effects model to predict choices based on the difference in reward 
# between the stimuli on each trial, and examine the interaction with condition (slow/fast).
if (experiment_name == 'experiment1') {
  choice_test_glme1 = glmer(choice ~ rescaled_reward_diff_z*prior + (1 |subject), family = binomial, data = DATA_TEST, control = cglmctrl)
} else if (experiment_name == 'experiment2') {
  choice_test_glme1 = glmer(choice ~ rescaled_reward_diff_z*prior + (1 + prior|subject), family = binomial, data = DATA_TEST, control = cglmctrl)
} else { # replication
  choice_test_glme1 = glmer(choice ~ rescaled_reward_diff_z*prior + (1 + rescaled_reward_diff_z*prior|subject), family = binomial, data = DATA_TEST, control = cglmctrl)
}

# remove non-significant main effects - Likelihood Ratio Test 
choice_test_glme1_comparison = drop1(choice_test_glme1, test = 'Chisq')
choice_test_glme1_comparison

# correct for multiple comparisons
# main effect of condition, interaction condition*reward
choice_test_glme1_comparison$`Pr(Chi)`
p.adjust(p = choice_test_glme1_comparison$`Pr(Chi)`, method = 'holm', n = 2) 

# Get the betas and confidence intervals
beta_se = sqrt(diag(vcov(choice_test_glme1))) # standard error for each coefficient (approx.)?
# table of estimates with 95% CI
betas = cbind(Est = fixef(choice_test_glme1), LL = fixef(choice_test_glme1) - 1.96 * beta_se, UL = fixef(choice_test_glme1) + 1.96 * beta_se)
betas #logit scale
# exp(betas) # log odds
# exp(betas) / (1 + (exp(betas))) # probability

# table for paper
xtable(betas, type = "latex")
VarCorr(choice_test_glme1)
# xtable(data.frame(VarCorr(choice_test_glme1))[1:2,c(1,2,5)])



# continue model reduction to obtain best model
if (experiment_name == 'experiment1') {
  choice_test_glme2 = glmer(choice ~ rescaled_reward_diff_z+prior + (1 |subject), family = binomial, data = DATA_TEST, control = cglmctrl)
} else if (experiment_name == 'experiment2') {
  choice_test_glme2 = choice_test_glme1
} else { # replication
  choice_test_glme2 = glmer(choice ~ rescaled_reward_diff_z+prior + (1 + rescaled_reward_diff_z*prior|subject), family = binomial, data = DATA_TEST, control = cglmctrl)
}
drop1(choice_test_glme2, test = 'Chisq')

if (experiment_name == 'experiment1') {
  choice_test_glme2 = glmer(choice ~ rescaled_reward_diff_z + (1 |subject), family = binomial, data = DATA_TEST, control = cglmctrl)
} else if (experiment_name == 'replication') {
  choice_test_glme2 = glmer(choice ~ rescaled_reward_diff_z + (1 + rescaled_reward_diff_z*prior|subject), family = binomial, data = DATA_TEST, control = cglmctrl)
}
drop1(choice_test_glme2, test = 'Chisq')

beta_se2 = sqrt(diag(vcov(choice_test_glme2))) # standard error for each coefficient (approx.)?
# table of estimates with 95% CI
betas2 = cbind(Est = fixef(choice_test_glme2), LL = fixef(choice_test_glme2) - 1.96 * beta_se2, UL = fixef(choice_test_glme2) + 1.96 * beta_se2)
betas2 #logit scale
# exp(betas2) # log odds
# exp(betas2) / (1 + (exp(betas2))) # probability

xtable(betas2, type = "latex")
VarCorr(choice_test_glme2)

