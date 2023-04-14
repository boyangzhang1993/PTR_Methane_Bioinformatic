


# Load required packages
library(lme4)
library(glmmLasso)


# Load the helper functions
source("split_data.R")
source("lasso.R")
# source("fit_model.R")


# Init
results_list <- vector("list", 15)
for (i in 1:15) {
  results_list[[i]] <- list()
}



# Fit analysis


fit_analysis = function(i_min, i_max, candidate_data, lambda) {

  
  # Loop over seeds and folds
  for (i_seed in c(i_min:i_max)) {
    
    AIC_vec <- rep(Inf, length(lambda))
    startTot = 6
    set.seed(i_seed)
    family = gaussian(link = "identity")
    BIC_vec <- rep(Inf, length(lambda))
    
    # Split data by cross-validation
    fold_data <- split_data(candidate_data)
    
    for (i_folder in c(1:1)) {
      # Split the data into training and testing sets (e.g., 80% training, 20% testing)
      test_data <- candidate_data[fold_data[i_folder,],]
      train_data <- candidate_data[fold_data[-i_folder,],]
      
      seed_fold_result = list()
      seed_fold_result[[1]] = i_seed
      seed_fold_result[[2]] = i_folder
      
      # Fit a LASSO model and collect result for the folder
      seed_fold_result <- run_lasso(train_data, test_data, 6, family = gaussian(link = "identity"), lambda, seed_fold_result)
      
      # Save the results

      results_list <- rbind(results_list, seed_fold_result)
    }
  }
  
  return(results_list)
  
  
}
