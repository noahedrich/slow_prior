# Models 
Contains the computational model code for the project: **An inductive bias for slowly changing features in human reinforcement learning**. 


## Contents
* All `.R` scripts can be run locally. Bash scripts (`.sh`) call on the `.R` scripts and can be used to run on a cluster. 

### Models
The scripts starting with `model_` contain the code that determines individual model behaviour. 
The Kalman Filters with knowlegde of the feature space (1LR, 1LR_cond, 2LR_feat, 2LR, 1LR_condC, 1LR_condSD) have a separate initialisation function `model_init.R`, the other models (basic, WSLS, random_choice, random_key) are initialised within their functions. 
All models with **1LR** in the name use the `model_1LR.R` script, their differences are specified within the script. Same for the models with **2LR** in the name. 

The models are: 
* **1LR**: One learning rate for all features. (1LR)
* **1LR_cond**: One learning rate in for 'slow' blocks and one learning rate for 'fast' blocks. (2LR_C in paper)
* **2LR_feat**: One learning rate for the slow feature in both block types and one learning rate for the fast condition in both block types. (2LR_F in paper)
* **2LR**: Separate learning rates for each feature (slow/fast) when it is relevant and irrelevant. Four learning rates in total. (4LR in paper)

* **1LR_condC**: One learning rate for all features, but the exploration parameter c is different for 'slow' and 'fast' blocks. (1LR_C in paper)
* **1LR_condSD**: One learning rate for all features, but the decision noise sigma is different for 'slow' and 'fast' blocks. (1LR_sigma in paper)
* **basic**: Kalman Filter model, which ignores the feature values, estimates only one single value using the information from the rewards.
* **random_choice**: makes random choices, with 1 parameter determining a bias to the accept or reject choice. 
* **random_key**: makes random choices, with 1 parameter determining a bias to one of the two response keys. 


### Model fitting 
* `fit_wrapper.sh`: Run this script for model fitting. It calls: 
   * `create_params_call.R`:
   * `fit.R`: Runs model fitting using `nloptr`. Set in this script whether fitting should maximise log likelihood or reward. 
      * `loss_func.R`: The loss function for the model fitting. 
   * `predict.R`: Saves the model predictions for the data, given the best fit model parameters found by `fit.R`. 

### Model recovery 
* `simulate_wrapper.sh`: Run this first. It simulates model choices using random parameter values. Calls these scripts: 
   *` recover_params.R`
   * `simulate.R`
   * `create_params_call.R`
* `recover_wrapper.sh`: Run this when the jobs from `simulate_wrapper.sh` are done. Runs model fitting on the simulated data. 

### Helper functions
* `encode_params.R`
* `encode_vector.R`
* `helper_funcs.R`
* `create_params.R`
