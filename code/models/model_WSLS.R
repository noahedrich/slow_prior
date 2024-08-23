model_WSLS = function(params, data) {
  ## A model that does win-stay loose-shift 
  
  # chooses first action at random 
  # if on previous trial chose accept and received reward >= 50 --> stays with accept with probability 1-*noise* (deterministic: 1) // switches to reject with probability *noise* (deterministic: 0)
  # if on previous trial chose accept and received reward <  50 --> switches to rejecting with probability 1-*noise* (deterministic: 1) // stays with accept with probability *noise* (deterministic: 0)
  # if on previous trial chose reject --> stays with reject with probability 1-*noise* (deterministic: 1) // switches to accept with probability *noise* (deterministic: 0)
  
  # initialise model
  model = NULL
  model$nblocks = length(unique(data$block))
  model$ntrials = length(data$block) / model$nblocks
  model$p_accept = model$choice = model$p_choice = model$LL =
    model$correct = model$received_reward = matrix(NA, model$ntrials+1, model$nblocks)
  # empty
  model$PE = model$V = model$V_slow = model$V_fast = model$Var = model$Var_slow = 
    model$Var_fast = NA
  
  # first trials random choice
  model$p_accept[1,] = 0.5
  
  # simulate model
  for (cblock in 1:model$nblocks) {
    cdata = subset(data, block == unique(data$block)[cblock])
    for (ctrial in 2:(model$ntrials+1)) {
      # convert previous p_accept into choice or get participant choice
      if (params$simulate_choices) {
        model$choice[ctrial-1,cblock] = rbinom(1, 1, model$p_accept[ctrial-1,cblock])
      } else {
        model$choice[ctrial-1,cblock] = cdata$choice[ctrial-1]
      }
      model$received_reward[ctrial-1,cblock] = c(0, 50, cdata$rescaled_reward[ctrial-1])[model$choice[ctrial-1,cblock]+2] # no choice, reject, accept
      
      # likelihood for participant choices
      model$p_choice[ctrial-1,cblock] = c(NA, 1-model$p_accept[ctrial-1,cblock], model$p_accept[ctrial-1,cblock])[cdata$choice[ctrial-1]+2] # no choice, reject, accept
      model$LL[ctrial-1,cblock] = log(model$p_choice[ctrial-1,cblock])
      
      # current trial choice
      # if previous choice accept
      if (model$choice[ctrial-1, cblock] == 1) { 
        if (model$received_reward[ctrial-1,cblock] >= 50) {
          model$p_accept[ctrial,cblock] = 1-params$random_switch # if previous choice accept and reward above 50 stay with accept
        } else {
          model$p_accept[ctrial,cblock] = params$random_switch # if previous choice accept and reward below 50 switch to reject
        }
      } else if(model$choice[ctrial-1, cblock] == 0) {
        model$p_accept[ctrial,cblock] = params$random_switch # if previous choice reject, switch to accept with probability random switch
        
      } else if(model$choice[ctrial-1, cblock] == -1) { 
        model$p_accept[ctrial,cblock] = 0.5 # if no previous choice, random
      }
    }
  }
  
  # check if choices correct
  model$correct[1:model$ntrials,] = (model$choice[1:model$ntrials,] == data$correct_choice)*1
  
  # ugly fix for removing last extra trial
  cut_last_trial = function(x, ntrials) {
    return(x[1:ntrials,])
  }
  model$p_accept =  model$p_accept[1:model$ntrials,]
  model$choice =  model$choice[1:model$ntrials,]
  model$p_choice =  model$p_choice[1:model$ntrials,]
  model$LL = model$LL[1:model$ntrials,]
  model$correct =  model$correct[1:model$ntrials,]
  model$received_reward  = model$received_reward[1:model$ntrials,]
  
  return(model)
}
