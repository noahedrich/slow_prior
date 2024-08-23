model_random_choice = function(params, data) {
  # Random choice model with a bias to either accept or reject (bias bounded between 0:reject and 1:accept). 
  
  # initialise model
  model = NULL
  model$nblocks = length(unique(data$block))
  model$ntrials = length(data$block) / model$nblocks
  model$p_accept = model$choice = model$p_choice = model$LL =
    model$correct = model$received_reward = matrix(NA, model$ntrials, model$nblocks)
  # empty
  model$PE = model$V = model$V_slow = model$V_fast = model$Var = model$Var_slow = 
    model$Var_fast = NA
  
  # simulate model
  model$p_accept[,] = params$bias_accept
  
  if (params$simulate_choices) {
    model$choice = matrix(rbinom(model$ntrials*model$nblocks, 1, model$p_accept), nrow = model$ntrials)
  } else {
    model$choice = matrix(data$choice, nrow = model$ntrials)
  }
  model$received_reward = data$rescaled_reward
  model$received_reward[model$choice == 0] = 50
  model$received_reward[model$choice == -1] = 0
  
  # likelihood for participant choices
  model$p_choice[,] = c(NA, 1 - params$bias_accept, params$bias_accept)[data$choice + 2] # no choice, reject, accept
  model$LL[,] = log(model$p_choice)
  
  # check if choices correct
  model$correct[1:model$ntrials,] = (model$choice[1:model$ntrials,] == data$correct_choice)*1
  return(model)
}
