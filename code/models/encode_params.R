encode_params = function(x, initvar, default_value, feature_len, model_name, simulate_choices) {
  # Builds list of parameters for different models from vector and model specifications. 
  x = as.numeric(as.vector(x))
  params = NULL
  
  model_split = strsplit(model_name, split = "_")[[1]]
  params$two_LR = "2LR" %in% model_split
  params$cond_LR = "cond" %in% model_split
  params$feat_LR = "feat" %in% model_split
  params$cond_c = "condC" %in% model_split
  params$cond_SD = "condSD" %in% model_split
  params$basic = "basic" %in% model_split
  params$random = "random" %in% model_split
  params$choice = "choice" %in% model_split
  params$WSLS = "WSLS" %in% model_split
  
  params$initvar = initvar # initial weight variance
  params$default_value = default_value # value of the reject option
  params$feature_len = feature_len # number of entries in the feature vec per feature
  
  params$simulate_choices = simulate_choices # simulate choices with model (T) or clamp to participant choices (F)
  
  if(params$random){ # random models 
    needed_len = 1
    if (params$choice) { # choice bias
      params$bias_accept = x[1]
    } else {# left/right key bias
      params$bias_right = x[1]
    }
  } else if(params$WSLS) { # win-stay loose-shift
    needed_len = 1
    params$random_switch = x[1]
  } else if(params$basic) { # basic KF model
    needed_len = 3
    
    params$c = x[1] # exploration/exploitation
    # params$beta = x[2] # temperature parameter for softmax
    params$choicesd = x[2] # sd in stim. value vs. default value comparison
    
    params$initalpha = x[3] # initial learning rate
    params$sigma = (params$initvar - params$initalpha*params$initvar)/params$initalpha # observation noise
    
  } else if (params$cond_c) { #  1LR, but with different *c* parameters by condition
    needed_len = 5
    
    params$kappa = x[1] # concentration of feature vector
    params$c_slow = x[2] # exploration/exploitation slow blocks
    params$c_fast = x[3] # exploration/exploitation fast blocks
    params$choicesd = x[4] # sd in stim. value vs. default value comparison
    params$initalpha = x[5] # initial learning rate
    params$sigma = (params$initvar - params$initalpha*params$initvar)/params$initalpha # observation noise
  } else if (params$cond_SD) { #  1LR, but with different *choicesd* parameters by condition
    needed_len = 5
    
    params$kappa = x[1] # concentration of feature vector
    params$c = x[2] # exploration/exploitation
    params$choicesd_slow = x[3] # sd in stim. value vs. default value comparison slow blocks
    params$choicesd_fast = x[4] # sd in stim. value vs. default value comparison fast blocks
    params$initalpha = x[5] # initial learning rate
    params$sigma = (params$initvar - params$initalpha*params$initvar)/params$initalpha # observation noise
  } else { # actual testing models
    needed_len = 3
    
    params$kappa = x[1] # concentration of feature vector
    params$c = x[2] # exploration/exploitation
    # params$beta = x[3] # temperature parameter for softmax
    params$choicesd = x[3] # sd in stim. value vs. default value comparison
    
    if (params$two_LR & !params$feat_LR) { # 2LR
      needed_len = needed_len+4
      params$initalpha_slow_rel = x[4] # initial learning rate, slow feature when it is relevant
      params$initalpha_slow_irrel = x[5] # slow feature when it is irrelevant
      params$initalpha_fast_rel = x[6] # fast feature when it is relevant
      params$initalpha_fast_irrel = x[7] # fast feature when it is irrelevant
      params$sigma_slow_rel = (params$initvar - params$initalpha_slow_rel*params$initvar)/params$initalpha_slow_rel # observation noise
      params$sigma_slow_irrel = (params$initvar - params$initalpha_slow_irrel*params$initvar)/params$initalpha_slow_irrel
      params$sigma_fast_rel = (params$initvar - params$initalpha_fast_rel*params$initvar)/params$initalpha_fast_rel
      params$sigma_fast_irrel = (params$initvar - params$initalpha_fast_irrel*params$initvar)/params$initalpha_fast_irrel
    } else if(params$cond_LR | params$feat_LR){ #1LR, but LR adapted to condition | one LR for slow one for fast
      needed_len = needed_len+2
      params$initalpha_slow = x[4] # initial learning rate
      params$initalpha_fast = x[5] 
      params$sigma_slow = (params$initvar - params$initalpha_slow*params$initvar)/params$initalpha_slow # observation noise
      params$sigma_fast = (params$initvar - params$initalpha_fast*params$initvar)/params$initalpha_fast
    } else { #1LR
      needed_len = needed_len+1
      params$initalpha = x[4] # initial learning rate
      params$sigma = (params$initvar - params$initalpha*params$initvar)/params$initalpha # observation noise
    }
  }
  
  if (needed_len != length(x))
    stop(paste("Parameter encoding failed:", needed_len, "parameters needed, but", length(x), "given.", sep = " "))
  
  return(params)
}
