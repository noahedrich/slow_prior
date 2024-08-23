# Meta-analysis of effects across studies 
# based on: Meta-Analysis with R (2015)

# !! make sure to set WD to the folder of this file before running, so that here() works
# !! here() should point to the 'code' folder

# LOAD EVERYTHING ----
library(here)
source(here('analysis', 'load.R'))

# define function ----
get_meta_vars = function(df, name, learning) {
  # Ne, the Number of participants (sample size) in the experimental treatment group,
  # Me, the Mean response among the participants in the experimental group,
  # Se, the Standard deviation of the response in the experimental group, and
  # Nc, Mc and Sc the sample size, mean response and standard deviation in the
  # control participants
  df = df %>%
    mutate(prior = ifelse(prior == 'slow', 'e', 'c')) 
  
  if (learning) { # if learning phase
    df = df %>%
      group_by(subject, block) %>%
      mutate(cum_reward = cumsum(received_reward),
             chance_cum_reward = cumsum(rep(50, length(received_reward))),
             cum_reward_chance = cum_reward - chance_cum_reward,
             final_cum_reward = cum_reward_chance[trial_number_block == max(trial_number_block)]) %>%
      select(subject, prior, block, final_cum_reward) %>%
      unique() %>%
      group_by(subject, prior) %>%
      summarise(mean_outcome = mean(final_cum_reward), .groups='drop_last')
  } else { # if test phase 
    df = df %>%
      group_by(subject, prior) %>%
      summarise(mean_outcome = mean(correct), .groups='drop_last')
  }
  df = df %>%
    group_by(prior) %>%
    summarise(M=mean(mean_outcome), S = sd(mean_outcome), N = n()) %>% 
    pivot_wider(names_from = prior, values_from = c(M, S, N), names_sep = '') %>%
    mutate(name = name)
  
  return(df)
}

# load data ----
data_exp1 = DATA %>% filter(experiment == 'experiment1')
data_exp1_test = DATA_TEST %>% filter(experiment == 'experiment1')

data_exp2 = DATA %>% filter(experiment == 'experiment2')
data_exp2_test =  DATA_TEST %>% filter(experiment == 'experiment2')

data_rep = DATA %>% filter(experiment == 'replication')
data_rep_test = DATA_TEST %>% filter(experiment == 'replication')


# fixed-effect meta-analysis ----
# learning trials ----
# get summary statistics
exp1_learning = get_meta_vars(data_exp1, 'experiment 1', learning=T)
exp2_learning = get_meta_vars(data_exp2, 'experiment 2', learning=T)
rep_learning = get_meta_vars(data_rep, 'replication', learning=T)

df_learning = rbind(exp1_learning, exp2_learning, rep_learning)

# do meta-analysis
m_learning = metacont(Ne, Me, Se, Nc, Mc, Sc, data=df_learning, sm = 'SMD')
m_learning
# Produce forest plot
# simple
forest(m_learning, xlab="slow vs. fast\ncum. reward - chance ",  
       studlab = c("Experiment 1", "Experiment 2", "Replication"),
       weight.study = "common", 
       leftcols = c("studlab"), leftlabs = c('Sample'),
       text.common = 'Overall',
       smlab="Hedge's g (95% CI)",
       random=F,
       rightcols = F, 
       hetstat = F, 
       addrow = 1,
       addrow.overall=F,
       col.diamond = prior_palette['slow'],
       col.square = c('grey', prior_palette['slow'], prior_palette['slow']),
       plotwidth = '4cm',
       # file = paste(save_path, 'participants_LP_forest.svg', sep = .Platform$file.sep),
       # func.gr = 'svg'
       )
# full
forest(m_learning, xlab="slow vs. fast\ncum. reward - chance ",  
       studlab = c("Experiment 1", "Experiment 2", "Replication"),
       weight.study = "common", 
       leftcols = c("studlab", "n.e", "mean.e", "sd.e", "mean.c", "sd.c","effect.ci", "w.common"), leftlabs = c('Sample', 'N', 'M', 'SD', 'M', 'SD', "Hedge's g (95% CI)", 'Weight'),
       text.common = 'Overall',
       smlab="Hedge's g (95% CI)",
       label.e = 'Slow Blocks',
       label.c = 'Fast Blocks',
       random=F,
       rightcols = F,
       addrow = 1,
       addrow.overall=1,
       addrows.below.overall = 1,
       digits.sd = 1,
       digits.mean = 1,
       col.diamond = prior_palette['slow'],
       col.square = c('grey', prior_palette['slow'], prior_palette['slow']),
       plotwidth = '6.5cm',
       width = 7,
       fontsize = 11,
       colgap.left = '3mm',
       # file = paste(save_path, 'participants_LP_forest_full.svg', sep = .Platform$file.sep),
       # func.gr = 'svg',
)

# test trials ----
# get summary statistics
exp1_test = get_meta_vars(data_exp1_test, 'experiment 1', learning=F)
exp2_test = get_meta_vars(data_exp2_test, 'experiment 2', learning=F)
rep_test = get_meta_vars(data_rep_test, 'replication', learning=F)
df_test = rbind(exp1_test, exp2_test, rep_test)

# do meta-analysis
m_test = metacont(Ne, Me, Se, Nc, Mc, Sc, data=df_test, sm="SMD")
m_test
# simple
# Produce forest plot
forest(m_test, xlab="slow vs. fast\n% correct", 
       studlab = c("Experiment 1", "Experiment 2", "Replication"),
       weight.study = "common", 
       leftcols = c("studlab"), leftlabs = c('Sample'),
       text.common = 'Overall',
       smlab="Hedge's g (95% CI)",
       random=F,
       rightcols = F, 
       hetstat = F, 
       addrow = 1,
       addrow.overall=F,
       col.diamond = 'grey',
       col.square = c('grey', prior_palette['slow'], 'grey'),
       plotwidth = '4cm',
       # file = paste(save_path, 'participants_TP_forest.svg', sep = .Platform$file.sep),
       # func.gr = 'svg'
)

# full
forest(m_test, xlab="slow vs. fast\n% correct", 
       studlab = c("Experiment 1", "Experiment 2", "Replication"),
       weight.study = "common", 
       leftcols = c("studlab", "n.e", "mean.e", "sd.e", "mean.c", "sd.c","effect.ci", "w.common"), leftlabs = c('Sample', 'N', 'M', 'SD', 'M', 'SD', "Hedge's g (95% CI)", 'Weight'),
       text.common = 'Overall',
       smlab="Hedge's g (95% CI)",
       label.e = 'Slow Blocks',
       label.c = 'Fast Blocks',
       random=F,
       rightcols = F,
       addrow = 1,
       addrow.overall=1,
       addrows.below.overall = 1,
       digits.sd = 3,
       digits.mean = 2,
       col.diamond = 'grey',
       col.square = c('grey', prior_palette['slow'], 'grey'),
       plotwidth = '6.5cm',
       width = 7,
       fontsize = 11,
       colgap.left = '3mm',
       # file = paste(save_path, 'participants_TP_forest_full.svg', sep = .Platform$file.sep),
       # func.gr = 'svg',
)
