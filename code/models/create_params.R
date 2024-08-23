create_params = function(model, n_values){
  # create parameter settings for each model type for model fitting!
  # sets starting values and lower and upper bounds
  
  settings = list()
  model_split = strsplit(model, split = "_")[[1]]
  settings$two_LR = "2LR" %in% model_split
  settings$cond_LR = "cond" %in% model_split
  settings$feat_LR = "feat" %in% model_split
  settings$cond_c = "condC" %in% model_split
  settings$cond_SD = "condSD" %in% model_split
  settings$basic = "basic" %in% model_split
  settings$random = "random" %in% model_split
  settings$choice = "choice" %in% model_split
  settings$WSLS = "WSLS" %in% model_split
  
  if(settings$random){ # random models 
    if (settings$choice) { # choice bias
      params = c('bias_accept')
      ub = c(1)
      lb = c(0)
    } else {# left/right key bias
      params = c('bias_right')
      ub = c(1)
      lb = c(0)
    }
  } else if(settings$WSLS) { # win-stay loose-shift model
    params = c('random_switch')
    ub = c(1)
    lb = c(0)
  } else if(settings$basic) { # basic KF model
    params = c('c', 'choicesd', 'initalpha')
    ub = c(30, 100, 1)
    lb = c( 0, 1, 0)
  } else if (settings$cond_c) { #  1LR, but with different *c* parameters by condition
    params = c('kappa', 'c_slow', 'c_fast', 'choicesd', 'initalpha')
    ub = c(30, 30, 30, 100, 1)
    lb = c(0, 0, 0, 1, 0)
  } else if (settings$cond_SD) { #  1LR, but with different *choicesd* parameters by condition
    params = c('kappa',  'c', 'choicesd_slow', 'choicesd_fast', 'initalpha')
    ub = c(30, 30, 100, 100, 1)
    lb = c(0, 0, 1, 1, 0)
  } else { # actual testing models
    # parameters shared among all models
    params = c('kappa', 'c', 'choicesd')
    ub = c(30, 30, 100)
    lb = c(0, 0, 1)
    
    if (settings$two_LR & !settings$feat_LR) {
      params = c(params, 'initalpha_slow_rel', 'initalpha_slow_irrel', 'initalpha_fast_rel', 'initalpha_fast_irrel')
      ub = c(ub, 1, 1, 1, 1)
      lb = c(lb, 0, 0, 0, 0)
    } else if (settings$cond_LR | settings$feat_LR) {
      params = c(params, 'initalpha_slow', 'initalpha_fast')
      ub = c(ub, 1, 1)
      lb = c(lb, 0, 0)
    } else {
      params = c(params, 'initalpha')
      ub = c(ub, 1)
      lb = c(lb, 0)
    }
  }
  
  # create df to save the parameter settings
  bounds = data.frame(t(c(lb, ub)))
  colnames(bounds) = c(paste0("lb_", params), paste0("ub_", params))
  bounds = bounds[rep(1, each = n_values),]
  
  # sample random starting values within bounds
  output = list()
  for (i in seq_along(params)) {
    output[[paste0("x0_", params[i])]] = runif(n_values, min = lb[i], max = ub[i])
  }
  
  output = data.frame(output)
  output = cbind(output, bounds)
  
  return(output)
}
