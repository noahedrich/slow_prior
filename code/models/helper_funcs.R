# Helper Functions
library(dplyr)

# standard error ----
se = function(x, na.rm = T) sd(x, na.rm = na.rm)/sqrt(sum(!is.na(x)))

# z-score ----
zscore = function(x) (x-mean(x))/sd(x)

# calculate information criterion ----
calc_AICc = function(LL, nparams, nsample) 2*nparams - 2*LL + (2*nparams*(nparams+1))/(nsample - nparams - 1)

# angular distance ----
angle_dist = function(target, test, max = 180, min = -180) {
  if (any(is.na(target), is.na(test))) {
    return(NA)
  }
  dist = target - test
  dist[dist>max] = dist[dist>max]-(max*2)
  dist[dist<min] = dist[dist<min]+(max*2)
  return(abs(dist))
}

# rolling mean ----
rolling_mean = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 2)}

# create stimuli sequences ----
resample = function(x, ...) x[sample.int(length(x), ...)]

create_seqs = function(feature_len, reps, condition, sd_slow = NA){
  # create stimuli sequences for each condition: 'slow' or 'fast'  (i.e. congruent/incongruent)
  # slow: samples step-size from gaussian with sd = sd_slow (30)
  # fast: samples step-sizes randomly, prevents smallest step-size
  # all feature positions (n = feature_len) are used before they repeat
  # both conditions: there is never a step-size of 0 (no change)
  
  if (condition == 'slow' & is.na(sd_slow)) stop("If condition == 'slow', must specify sd_slow.")
  
  for (b in 1:reps) {
    # vector of positions for current block
    pos = sample(seq(0, 359, by = 360/feature_len))
    # randomly sample starting position for first block & remove value
    if (b == 1) {
      out = sample(pos, 1)
      pos = pos[-which(pos == out)]
    }
    
    for (i in 1:length(pos)) {
      # distance between current position and possible future positions
      dist = angle_dist(out[length(out)], pos)
      # generate probability density function over dist
      if (condition == 'slow') prob = dnorm(dist, mean = 0, sd = sd_slow)
      if (condition == 'fast') prob = ifelse(dist <= 360/feature_len, exp(-100), 1)
      # select next value according to prob
      out = c(out, resample(pos, 1, prob = prob))
      
      # prevent same value from repeating, (only possible at the start of a new repetition)
      while (out[length(out)] == out[length(out)-1]) out[length(out)] = resample(pos, 1, prob = prob)
      # remove used value
      pos = pos[-which(pos == out[length(out)])]
    }
  }
  
  # prevent smallest step-size in fast sequences
  if (condition == 'fast') {
    step_sizes = angle_dist(out[-1], na.omit(dplyr::lag(out)))
    while (any(step_sizes <= 360/feature_len)) {
      old_i = which(step_sizes <= 360/feature_len)
      vals = out[old_i]
      out = out[-old_i]
      for (v in vals) {
        out = append(out, v, after = sample(length(out), 1))
      }
      step_sizes = angle_dist(out[-1], na.omit(dplyr::lag(out)))
    }
  }
  return(out)
}

# create simulated data ----
create_data = function(feature_len, reps, sd_slow, nblocks, nsub) {
  if (nblocks %% 2 != 0) stop("nblocks should be an even number.")

  data = data.frame(matrix(nrow = nsub*nblocks*feature_len*reps, ncol = 14))
  colnames(data) = c('subject', 'block', 'trial_number_block', 'block_type', 
                     'slow', 'pos_color', 'pos_shape', 'pos_reward', 'prior', 
                     'rescaled_reward', 'correct_choice', 'label_right', 'choice', 'received_reward')
  for (i in 1:nsub) {
    nrows = nblocks*feature_len*reps
    i_sub = (1+(i-1)*nrows):(i*nrows)
    data[i_sub, 1] = paste0('sim_', i) # subject
    data[i_sub, 2] = rep(1:nblocks, each = feature_len*reps) # block
    data[i_sub, 3] = rep(1:(feature_len*reps), nblocks) # trial_number_block
    data[i_sub, 4] = 'col_congruent' # block_type, doesn't matter, just to be compatible with funcs for participant data
    data[i_sub, 5] = 'color' # slow, for compatibility
    pos_color = replicate(nblocks, create_seqs(feature_len = feature_len, reps = reps, condition = 'slow', sd_slow= sd_slow))
    pos_shape = replicate(nblocks, create_seqs(feature_len = feature_len, reps = reps, condition = 'fast'))
    dim(pos_color) = dim(pos_shape) = reps*feature_len*nblocks
    data[i_sub, 6] = pos_color # pos_color
    data[i_sub, 7] = pos_shape # pos_shape
    data[i_sub, 8] = rep(sapply(1:nblocks, sample, x = 0:359, size = 1), each = feature_len*reps) # pos_reward, in experiment only c(10, 190, 100, 280)
    data[i_sub, 9] = ifelse(data[i_sub, 2] <= nblocks/2, 'congruent', 'incongruent') # prior
    dist = ifelse(data[i_sub, 2] <= nblocks/2, angle_dist(data[i_sub, 8], data[i_sub, 6]), angle_dist(data[i_sub, 8], data[i_sub, 7]))
    data[i_sub,10] = floor((180 - dist)/180*100) # rescaled_reward, first half, colour/slow is rewarded, second half shape/fast is rewarded
    data[i_sub,11] = ifelse(data[i_sub, 10] > 50, 1, 0) # correct_choice
    
    # response side of 'Accept', left/right balanced within each block (needed for random_key model)
    for (j in 1:nblocks) {
      data$label_right[data$subject == paste0('sim_', i) & data$block == j & !is.na(data$block)] = sample(rep(c('Accept', 'Reject'), each = feature_len*reps/2)) # label_right
    }
    cat('.')
  }
  
  # empty choice and reward columns
  data[i_sub, 13] = NA # choice
  data[i_sub, 14] = NA # received_reward
  return(data)
}

# paired permutation test ----
# from: https://github.com/kbroman/broman/blob/main/R/permtest.R
paired.perm.test = function(d, n.perm=NULL, pval=TRUE){
    n = length(d)
    tobs = t.test(d)$statistic
    if(is.null(n.perm)) { # do exact test
      ind = binary.v(n)
      allt = apply(ind,2,function(x,y)
        t.test((2*x-1)*y)$statistic,d)
    }
    else { # do n.perm samples
      allt = 1:n.perm
      for(i in 1:n.perm)
        allt[i] = t.test(d*sample(c(-1,1),n,replace=TRUE))$statistic
    }
    if(pval) return(mean(abs(allt) >= abs(tobs)))
    attr(allt, "tobs") <- tobs
    allt
}
