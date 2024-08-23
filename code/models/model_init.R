model_init = function(params, data) {
  model = NULL
  # get data size info
  model$nblocks = length(unique(data$block))
  model$ntrials = length(data$block) / model$nblocks
  
  # empty model attributes 
  model$W = model$W_Var = model$alphas = 
    array(NA, dim = c(2*params$feature_len, model$ntrials+1, model$nblocks))
  model$V = model$V_slow = model$V_fast = model$Var = model$Var_slow = model$Var_fast = 
    model$V_accept = model$PE = model$LL = model$p_accept = model$p_choice = 
    model$choice = model$correct = model$received_reward = matrix(NA, model$ntrials, model$nblocks)
  
  # make feature vectors:
  # get stimuli positions - put slow feature first and fast feature second
  model$S = array(NA, dim = c(2, model$ntrials, model$nblocks))
  model$S[1,1:model$ntrials,] = ifelse(data$slow == "color", data$pos_color, data$pos_shape)
  model$S[2,1:model$ntrials,] = ifelse(data$slow == "color", data$pos_shape, data$pos_color)
  
  # convert stimuli positions to von Mises distributions
  model$X = apply(model$S, c(2,3), function(x) encode_vector(x, params$feature_len, params$kappa))
  
  # starting conditions for each block: 
  # initialise W such that prediction on first trial is == default value
  # can use any block in denominator, as all are normalised
  model$W[,1,] = params$default_value/sum(model$X[,1,1])
  # in case needed: apply(model$X[,1,], 2, function(x) params$default_value/sum(x))
  
  # initialise W_Var such that variance on first trial is set to param$initvar for vector/feature
  model$W_Var[,1,] = (2*params$initvar)/sum(model$X[,1,1])
  # (1 vs 2 LR models)
  # initialse the learning rate(s)
  if (params$two_LR & !params$feat_LR) { # 2LR
    model$alphas[,1,unique(data$block[data$prior == 'congruent'])] = 
      c(rep(params$initalpha_slow_rel, params$feature_len), #LR for slow feature, relevant
        rep(params$initalpha_fast_irrel, params$feature_len)) #LR for fast feature, irrelevant
    model$alphas[,1,unique(data$block[data$prior == 'incongruent'])] = 
      c(rep(params$initalpha_slow_irrel, params$feature_len), #LR for slow feature, irrelevant 
        rep(params$initalpha_fast_rel, params$feature_len)) #LR for fast feature
  } else if(params$cond_LR) { # 1LR, but different LRs for condition
    model$alphas[,1,unique(data$block[data$prior == 'congruent'])] = params$initalpha_slow
    model$alphas[,1,unique(data$block[data$prior == 'incongruent'])] = params$initalpha_fast
  } else if (params$feat_LR) { # 1LR, but one LR for each *feature* irrespective of condition
    model$alphas[1:params$feature_len,1,] = params$initalpha_slow
    model$alphas[(params$feature_len+1):(2*params$feature_len),1,] = params$initalpha_fast
  } else { # 1LR
    model$alphas[,1,] = params$initalpha
  }
  return(model)
}
