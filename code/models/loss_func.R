loss_func = function(x, data, initvar, default_value, feature_len, model_name, simulate_choices, max_LL = T) {
  # runs model with given parameters and returns sum negative log likelihood
  params = encode_params(x, initvar, default_value, feature_len, model_name, simulate_choices)
  
  # run functions 
  if(params$random){ # random models 
    if (params$choice) { # choice bias
      model = model_random_choice(params, data)
    } else { # left/right key bias
      model = model_random_key(params, data)
    }
  } else if(params$WSLS) { # win-stay loose-shift
    model = model_WSLS(params, data)
  } else if(params$basic) { # basic Kalman Filter
    model = model_basic(params, data)
  } else if(params$fixed_LR) { # fixed LR mode
    model = model_init(params = params, data = data)
    model = model_1LR_fixedLR(params = params, data = data, model = model)
  } else { # actual testing models
    model = model_init(params = params, data = data)
    
    if (params$two_LR) { # 2LR, 2LR_feat
      model = model_2LR(params = params, data = data, model = model)
    } else { # 1LR, 1LR_cond, 1LR_condC, 1LR_condSD
      model = model_1LR(params = params, data = data, model = model)
    }
  }
  LL = -sum(model$LL, na.rm = T)
  R = -as.numeric(sum(model$received_reward, na.rm = T))
  
  # save fitting history Append to the results data frame 
  # <<- modifies variable in the parent environment
  fitting_history <<- rbind(fitting_history, cbind(t(x), LL, R))
  
  if (max_LL) return(LL) else return(R)
}
