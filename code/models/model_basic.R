model_basic = function(params, data) {
  ## Kalman Filter model without knowledge of feature space
  
  # initialise model
  model = NULL
  model$nblocks = length(unique(data$block))
  model$ntrials = length(data$block) / model$nblocks
  model$V_accept = model$PE = model$p_accept = model$p_choice = model$LL = 
    model$choice = model$correct = model$received_reward = 
    matrix(NA, model$ntrials, model$nblocks)
  model$V = model$Var = model$alphas = matrix(NA, model$ntrials+1, model$nblocks)
  # empty
  model$V_slow = model$V_fast = model$Var_slow = model$Var_fast = NA
  
  # starting conditions for each block
  model$V[1,] = params$default_value
  model$Var[1,] = params$initvar
  model$alphas[1,] = params$initalpha
  
  for (cblock in 1:model$nblocks) {
    cdata = subset(data, block == unique(data$block)[cblock])
    for (ctrial in 1:model$ntrials) {
      # calculate value and probability of accepting (UCB)
      model$V_accept[ctrial, cblock] = model$V[ctrial, cblock] + params$c * model$Var[ctrial, cblock]
      probability_accept = pnorm(model$V_accept[ctrial, cblock], mean = params$default_value, sd = params$choicesd)
      probability_accept = max(probability_accept,  1e-5)
      probability_accept = min(probability_accept,  1-1e-5)
      model$p_accept[ctrial, cblock] = probability_accept
      
      if (params$simulate_choices) { # if needed, simulate choice
        model$choice[ctrial, cblock] = (model$p_accept[ctrial, cblock] > 0.5)*1
      } else { # otherwise, use participant choice
        model$choice[ctrial, cblock] = cdata$choice[ctrial]
      }
      model$received_reward[ctrial, cblock] = ifelse(model$choice[ctrial, cblock] == 0, 50, cdata$rescaled_reward[ctrial])
      
      # likelihood for participant choices
      if (!is.na(cdata$choice[ctrial]) & cdata$choice[ctrial] > -1) { 
        model$p_choice[ctrial, cblock] = c(1 - model$p_accept[ctrial, cblock], model$p_accept[ctrial, cblock])[cdata$choice[ctrial] + 1]
        model$LL[ctrial, cblock] = log(model$p_choice[ctrial, cblock])
      }
      
      # prepare for next trial
      if (model$choice[ctrial, cblock] == 1) {
        # calculate PE and update weights
        model$PE[ctrial, cblock] = model$received_reward[ctrial, cblock]-model$V[ctrial, cblock]
        model$V[ctrial+1,cblock] = model$V[ctrial,cblock] + model$alphas[ctrial, cblock]*model$PE[ctrial, cblock]
        
        # update learning rate (kalman gain = alpha) and variance
        model$alphas[ctrial+1, cblock] = model$Var[ctrial, cblock] / (model$Var[ctrial, cblock] + params$sigma)
        model$Var[ctrial+1, cblock] = model$Var[ctrial, cblock] - model$alphas[ctrial, cblock] * model$Var[ctrial, cblock]
      } else {
        # if reject or no response: no learning
        model$V[ctrial+1, cblock] = model$V[ctrial, cblock]
        model$alphas[ctrial+1, cblock] = model$alphas[ctrial, cblock]
        model$Var[ctrial+1, cblock] = model$Var[ctrial, cblock]
      }
    }
  }
  # check if choices correct
  model$correct[1:model$ntrials,] = (model$choice[1:model$ntrials,] == data$correct_choice)*1
  
  # remove extra row
  model$V = model$V[-nrow(model$V),]
  model$Var = model$Var[-nrow(model$Var),]
  model$alphas = model$alphas[-nrow(model$alphas),]
  return(model)
}
