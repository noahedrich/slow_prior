model_2LR = function(params, data, model) {
  ## Two learning rate model - with Kalman Filter 
  
  slow_i = 1:params$feature_len
  fast_i = (1+params$feature_len):(2*params$feature_len)
  for (cblock in 1:model$nblocks) {
    cdata = subset(data, block == unique(data$block)[cblock])
    
    # adjust *LR* to feature type if necessary (2LR_feat)
    if (params$feat_LR) {
      sigma_slow = params$sigma_slow
      sigma_fast = params$sigma_fast
    } else { # 2LR
      sigma_slow = ifelse(cdata$prior[1] == 'congruent', params$sigma_slow_rel,   params$sigma_slow_irrel)
      sigma_fast = ifelse(cdata$prior[1] == 'congruent', params$sigma_fast_irrel, params$sigma_fast_rel)
    }
    for (ctrial in 1:model$ntrials) {
      # get value & variance
      model$V[ctrial, cblock] = c(model$W[,ctrial,cblock] %*% model$X[,ctrial,cblock])
      model$Var[ctrial, cblock] = c(model$W_Var[,ctrial,cblock] %*% model$X[,ctrial,cblock])
      
      # slow/fast feature V & Var
      model$V_slow[ctrial, cblock] = c(model$W[slow_i,ctrial,cblock] %*% model$X[slow_i,ctrial,cblock])
      model$Var_slow[ctrial, cblock] = c(model$W_Var[slow_i,ctrial,cblock] %*% model$X[slow_i,ctrial,cblock])
      model$V_fast[ctrial, cblock] = c(model$W[fast_i,ctrial,cblock] %*% model$X[fast_i,ctrial,cblock])
      model$Var_fast[ctrial, cblock] = c(model$W_Var[fast_i,ctrial,cblock] %*% model$X[fast_i,ctrial,cblock])
      
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
        model$W[,ctrial+1,cblock] = model$W[,ctrial,cblock] + model$alphas[,ctrial, cblock]*model$PE[ctrial, cblock]*model$X[,ctrial,cblock]
        
        # update learning rate (kalman gain = alpha) and variance
        model$alphas[slow_i, ctrial+1, cblock] = model$Var_slow[ctrial, cblock] / (model$Var_slow[ctrial, cblock] + sigma_slow)
        model$alphas[fast_i, ctrial+1, cblock] = model$Var_fast[ctrial, cblock] / (model$Var_fast[ctrial, cblock] + sigma_fast)
        model$W_Var[,ctrial+1,cblock] = model$W_Var[,ctrial,cblock] - model$alphas[,ctrial, cblock] * model$X[,ctrial,cblock] * model$W_Var[,ctrial,cblock]
      } else {
        # if reject or no response: no learning
        model$W[,ctrial+1,cblock] = model$W[,ctrial,cblock]
        model$alphas[,ctrial+1, cblock] = model$alphas[,ctrial, cblock]
        model$W_Var[,ctrial+1,cblock] = model$W_Var[,ctrial,cblock]
      }
    }
  }
  # check if choices correct
  model$correct[1:model$ntrials,] = (model$choice[1:model$ntrials,] == data$correct_choice)*1
  return(model)
}
