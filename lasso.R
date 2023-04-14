run_lasso <- function(train_data,
                      test_data,
                      startTot,
                      lambda,
                      family,
                      seed_fold_result) {
  # Initialize a vector to store BIC values for each lambda
  BIC_vec <- rep(Inf, length(lambda))
  
  
  ## Scale and Center
  train = train_data
  test = test_data
  
  scaled_train = scale(train_data[, c(startTot:ncol(train))], center = T, scale = T)
  train[, c(startTot:ncol(train))] = scaled_train
  
  
  scaled.new <- scale(
    test_data[, c(startTot:ncol(train))],
    attr(scaled_train, "scaled:center"),
    attr(scaled_train, "scaled:scale")
  )
  test[, c(startTot:ncol(train))] = scaled.new
  ## Refilter scaled data
  col.nas.sheep <-
    find_nas(data_nas = train,
             col_need = TRUE,
             row_need = FALSE)
  col.nas.sheep2 <-
    find_nas(data_nas = test,
             col_need = TRUE,
             row_need = FALSE)
  na_col = append(col.nas.sheep, col.nas.sheep2)
  na_col = unique(na_col)
  if (length(na_col) > 0) {
    test = test[,-na_col]
    train = train[,-na_col]
  }
  
  ## Formula builder
  name_others <- c(
    which(names(train) == "ANIMID"),
    which(names(train) == "SEX"),
    which(names(train) == "SampleID"),
    which(names(train) == "CH4"),
    which(names(train) == "Breed"),
    which(names(train) == "CH4DMI")
  )
  
  name.variables.sheep <- names(train)[-name_others]
  
  Formula_GlmmLasso <-
    formula(paste(
      "CH4 ~ as.factor(Breed) + as.factor(SEX) +",
      paste(name.variables.sheep, collapse = " + ")
    ))
  glm_test <- try(glmmLasso(
    fix = Formula_GlmmLasso,
    data = train,
    rnd = list(ANIMID = ~ 1),
    lambda = 50
  ))
  amount_variables <-
    length(name.variables.sheep) + length(unique(train$Breed)) + length(unique(train$SEX))
  # In rare situation, bread or sex are correlated with the random effect which causes not full rank error. Thus first check the glmlasso and then reset the formula.
  if (class(glm_test) == "try-error") {
    Formula_GlmmLasso <-
      formula(paste(
        "CH4 ~as.factor(SEX)+",
        paste(name.variables.sheep, collapse = " + ")
      ))
    amount_variables <-
      length(name.variables.sheep) + length(unique(train$SEX))
    glm_test <-
      try(glmmLasso(
        fix = Formula_GlmmLasso,
        data = train,
        rnd = list(ANIMID = ~ 1),
        lambda = 50
      ))
    if (class(glm_test) == "try-error") {
      Formula_GlmmLasso <-
        formula(paste("CH4 ~", paste(name.variables.sheep, collapse = " + ")))
      amount_variables <- length(name.variables.sheep)
    }
  }
  
  
  
  
  
  # Create initial values for Delta matrix
  Delta.start <-
    as.matrix(t(rep(0, amount_variables + length(
      levels(train$ANIMID)
    ))))
  
  # Initialize the starting value for Q
  Q.start <- 0.1
  
  # Iterate through each lambda value
  for (x in 1:length(lambda)) {
    # Print progress at regular intervals
    if (x %% floor(length(lambda) / 5) == 0) {
      print(x)
    }
    
    # Try fitting the glmmLasso model with the current lambda value
    glm1 <- try(glmmLasso(
      fix = Formula_GlmmLasso,
      data = train,
      rnd = list(ANIMID = ~ 1),
      lambda = lambda[x],
      switch.NR = T,
      final.re = TRUE,
      family = family,
      control = list(start = Delta.start[x,], q_start =
                       Q.start[x])
    ),
    silent = FALSE)
    
    # If the model was successfully fitted, store the BIC value
    if (class(glm1) != "try-error") {
      BIC_vec[x] <- glm1$bic
      
      # Update the Delta matrix and Q values for the next iteration
      Delta.start <-
        rbind(Delta.start, glm1$Deltamatrix[glm1$conv.step,])
      Q.start <- c(Q.start, glm1$Q_long[[glm1$conv.step + 1]])
      
    } else {
      # If the model did not fit, set BIC to Inf for this lambda value
      BIC_vec[x] <- Inf
      print("no")
    }
  }
  opt <- which.min(BIC_vec)
  lambda_sheep_BIC_final <- lambda[opt]
  
  seed_fold_result[[3]] = lambda_sheep_BIC_final
  glm_lasso_optimal_BIC <- glmmLasso(
    fix = Formula_GlmmLasso,
    data = train,
    rnd = list(ANIMID = ~ 1),
    lambda = lambda_sheep_BIC_final,
    switch.NR = T,
    final.re = TRUE,
    family = family,
    control = list(start = Delta.start[opt,], q_start =
                     Q.start[opt])
  )
  
  glm_lasso_optimal_BIC_summary <- summary(glm_lasso_optimal_BIC)
  glm_lasso_optimal_BIC_coefficients <-
    glm_lasso_optimal_BIC_summary$coefficients
  
  seed_fold_result[[4]] = glm_lasso_optimal_BIC_coefficients
  
  # Collect names of variables and remove factor variables for refit
  index_not_selected <- c()
  n_not_selected = 0
  for (i in 1:nrow(glm_lasso_optimal_BIC_coefficients)) {
    j = 4
    
    if (is.na(glm_lasso_optimal_BIC_coefficients[i, j]) == TRUE) {
      n_not_selected = n_not_selected + 1
      index_not_selected[n_not_selected] <- i
    }
    
  }
  index_not_selected
  
  rownames(glm_lasso_optimal_BIC_coefficients[-index_not_selected,])
  
  seed_fold_result[[5]] = rownames(glm_lasso_optimal_BIC_coefficients[-index_not_selected,])
  
  
  if (length(index_not_selected) - nrow(glm_lasso_optimal_BIC_coefficients) == 0) {
    FormulaNew = formula("CH4 ~ (1|ANIMID)")
  } else{
    if (length(index_not_selected) != 0) {
      selected_coef <-
        glm_lasso_optimal_BIC_coefficients[-index_not_selected,]
    } else{
      selected_coef <- glm_lasso_optimal_BIC_coefficients[-1,]
    }
    selected_coef_reordered_p_value <-
      selected_coef[order(selected_coef[, 4]),]
    seed_fold_result[[6]] = selected_coef_reordered_p_value
    
    signifcant_variables = selected_coef_reordered_p_value[as.numeric(which(selected_coef_reordered_p_value[, 4] < 0.05)),]
    
    
    
    significant_variables_names = row.names(signifcant_variables)[-1]
    seed_fold_result[[7]] = significant_variables_names
    
  }
  breed <-
    which(str_detect(significant_variables_names, "Breed"))
  sex_check <-
    which(str_detect(significant_variables_names, "SEX"))
  if (length(breed) == 0 & length(sex_check) == 0) {
    FormulaNew = formula(paste(
      "CH4 ~ (1|ANIMID)+",
      paste(significant_variables_names, collapse = " + ")
    ))
    
  }
  if (length(breed) != 0 & length(sex_check) != 0) {
    significant_variables_names <- significant_variables_names[-append(breed, sex_check)]
    FormulaNew = formula(paste(
      "CH4 ~ (1|ANIMID)+as.factor(Breed) + as.factor(SEX) +",
      paste(significant_variables_names, collapse = " + ")
    ))
    
  }
  
  
  if (length(breed) != 0) {
    significant_variables_names <- significant_variables_names[-breed]
    FormulaNew = formula(paste(
      "CH4 ~ (1|ANIMID)+as.factor(Breed)+",
      paste(significant_variables_names, collapse = " + ")
    ))
    
  }
  
  if (length(sex_check) != 0) {
    significant_variables_names <- significant_variables_names[-sex_check]
    FormulaNew = formula(paste(
      "CH4 ~ (1|ANIMID)+as.factor(SEX)+",
      paste(significant_variables_names, collapse = " + ")
    ))
    
  }
  if (length(significant_variables_names) == 0) {
    FormulaNew = formula("CH4 ~ (1|ANIMID)")
  }
  # Refit to a linear mixed model
  seed_fold_result[[8]] = FormulaNew
  glmModel = lmer(formula = FormulaNew, data = train)
  summary_train_model = summary(glmModel)
  summary_train_coef = summary_train_model$coefficients
  # View(summary_train_coef)
  seed_fold_result[[9]] = summary_train_coef
  prediction = predict(glmModel,
                       newdata = test,
                       allow.new.levels = T)
  observation = test$CH4
  error <- prediction - observation
  error_2 <- error ^ 2
  MSPE.2 <- mean(error_2)
  MSPE <- MSPE.2
  rmspe <- sqrt(MSPE.2)
  
  RSR = rmspe / sd(observation)
  tmp.ccc <-
    epi.ccc(
      x = observation ,
      y = prediction ,
      ci = "z-transform",
      conf.level = 0.95,
      rep.measure = FALSE,
      subjectid
    )
  tmp.lab <- data.frame(lab = paste(
    "CCC: ",
    round(tmp.ccc$rho.c[, 1], digits = 2),
    " (95% CI ",
    round(tmp.ccc$rho.c[, 2], digits = 2),
    " - ",
    round(tmp.ccc$rho.c[, 3], digits = 2),
    ")",
    sep = ""
  ))
  
  # Return the results values
  seed_fold_result[[3]] = lambda_sheep_BIC_final
  seed_fold_result[[4]] = glm_lasso_optimal_BIC_coefficients
  seed_fold_result[[5]] = rownames(glm_lasso_optimal_BIC_coefficients[-index_not_selected,])
  seed_fold_result[[10]] = observation
  seed_fold_result[[11]] = prediction
  seed_fold_result[[12]] = MSPE
  seed_fold_result[[13]] = error_2
  seed_fold_result[[14]] = RSR
  seed_fold_result[[15]] = BIC_vec[opt]
  
  return(seed_fold_result)
}
