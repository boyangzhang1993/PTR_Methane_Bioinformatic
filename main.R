
# Library

library(lme4)
library(SIS)
library(epiR)
library(stringr)
library(glmmLasso)


# Load data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

asv_sheep = read.csv('asv_sheep_classified.csv')

# Load functions
source("data_manipulate.R")
source("fit_data.R")

# Factor variables
asv_sheep$SEX = as.factor(asv_sheep$SEX)
asv_sheep$Breed = as.factor(asv_sheep$Breed)
asv_sheep$ANIMID = as.factor(asv_sheep$ANIMID)
# Seed parameter 
i_min = 10
i_max = 12
max_lambda = 500
min_lambda = 0
step = 1
lambda = seq(max_lambda, min_lambda, -step)
saveFileName = paste("LASSO", i_min, i_max,"Lambda",max_lambda,step,sep = "_")
startCol = 9 

# Remove 0s
asv_sheep <- remove_zeros(asv_sheep, startCol)
asv_sheep <- filter_nonzero_count_cols(asv_sheep, startCol, 1)

# Log transformation
asv_sheep[, startCol:ncol(asv_sheep)] = log(1 + asv_sheep[, startCol:ncol(asv_sheep)])
# SIS test
candidcate_var <- sis_test(data = asv_sheep, start_col = startCol, response_var = "CH4")
candidate_data = asv_sheep[, candidcate_var]


# Lasso with control list

seed_fold_results = fit_analysis(i_min, i_max, candidate_data, lambda)

# Result and analysis
seed_fold_results
