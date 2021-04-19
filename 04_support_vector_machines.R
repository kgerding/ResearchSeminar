#########################################################
### Support Vector Machines -----------------------------------
#########################################################

library(data.table)
library(dplyr)
library(tidyverse)
library(e1071) # for SVM
library(tictoc) # to measure time elapsed



# SETUP ------------------------------------------------------

rm(list=ls())
setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')
house_only16_mv <- fread('./Data/2016.csv', drop = 'V1')

colClasses = c(num_bathroom = 'numeric',
               num_bedroom = 'numeric',
               area_live_finished = 'numeric_log',
               num_garage = 'numeric', 
               area_garage = 'numeric',
               flag_tub_or_spa = 'factor',
               loc_latitude = 'numeric',
               loc_longitude = 'numeric',
               area_lot = 'numeric_log',
               loc_city = 'numeric',
               loc_zip  = 'numeric',
               num_pool = 'numeric',
               num_story = 'numeric',
               flag_fireplace = 'factor',
               num_tax_building = 'numeric_log',
               num_tax_total = 'numeric_log',
               num_tax_property = 'numeric_log',
               age = 'numeric_log')

# make conversions
for (i in colnames(house_only16_mv)) {
  if (colClasses[i][[1]] == 'numeric') {
    house_only16_mv[[i]] <- as.numeric(house_only16_mv[[i]])
  } else if (colClasses[[i]] == 'factor') {
    house_only16_mv[[i]] <- as.factor(house_only16_mv[[i]])
  } else if (colClasses[[i]] == 'numeric_log'){
    house_only16_mv[[i]] <- log(as.numeric(house_only16_mv[[i]]))
  }
}



### PART 1: DATA PREPROCESSING ###--------------------------------------

# select the dataframe
data = (na.omit(house_only16_mv))

# use only first 10'000
data = (data[1:10000,])

# normalize area_garage
# log only if num_garage is not 0, to avoid having -inf from log(0)
area_garage_log = rep(0, nrow(data))
for (i in 1:nrow(data)){
  if (data$num_garage[i] > 0) { 
    area_garage_log[[i]] <- log(data$area_garage[i])
  }}
data$area_garage <- area_garage_log

# set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# features we want to omit for the model 
omit <- c('loc_latitude', 'loc_longitude', 'num_tax_total', 'num_tax_property')

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define training label = dependent variable
output_vector = as.matrix(log(train16[,'num_tax_building']))
test_vector = as.matrix(log(test16[,'num_tax_building']))

#omit variables and convert to numeric again
train16 <- train16 %>% dplyr::select(-omit)
test16 <- test16 %>% dplyr::select(-omit)

# Create a sparse matrix
train16_sparse <- data.frame(model.matrix(~ . -1, train16))
test16_sparse <- data.frame(model.matrix(~ . -1, test16))


# MODEL ---------------------
library(LiblineaR)

#Regression with SVM
tic()
modelsvm = svm(num_tax_total ~ ., data = train16_sparse,
               type = 'eps-regression',
               kernel = 'linear',
               scale = FALSE, # don't scale data as we already did
               shrinking = TRUE, # shrink variables
               epsilon = 0.3, 
               verbose = TRUE,
               cost = 100,
              cross = 5) # cross-validation
toc()

LiblineaR(num_tax_total ~ ., data = as.matrix(train16_sparse))

#Regression with LibineaR
# tic()
# modellbm = LiblineaR(target = output_vector, data = as.matrix(train16_sparse[,-8]),
#                type = 12, # support vector regression L2 regularized
#                epsilon = 0.3, 
#                svr_eps = 0.1,
#                verbose = TRUE,
#                cost = 100)
#                #cross = 5) 
# toc()

# Predict using SVM regression
pred_svm = predict(modelsvm, test16_sparse)
rmse_svr <- sqrt(mean((pred_svm - output_vector)^2))
r2_svr <- 1 - (sum((test_vector-pred_svm)^2) / sum((test_vector-mean(test_vector))^2) )


# Compare to a simple regression model ----------------------------------
train16_sparse <- data.frame(model.matrix(~ . -1, train16))
test16_sparse <- data.frame(model.matrix(~ . -1, test16))

# simple regression
model_lm = lm(num_tax_total ~ ., data = (train16_sparse))
summary(model_lm)

pred_lm <- predict(model_lm, test16_sparse)
summary(pred_lm)

# Predict using simple regression
rmse_lm <- sqrt(mean((pred_lm - test_vector)^2))
r2_lm <- 1 - (sum((test_vector-pred_lm)^2) / sum((test_vector-mean(test_vector))^2))
str(test_vector)
str(pred_lm)

## Tuning SVR model ---------------------------------------------------
# by varying values of maximum allowable epsilon and cost parameter
# tic()
# OptModelsvm=tune(svm, num_tax_total ~ ., data = train16_sparse,
#                  type = 'eps-regression', #e-insensitive loss regression
#                  kernel = 'linear', 
#                  scale = FALSE,
#                  cross = 5,
#                  epsilon = 0.3,
#                  ranges=list(cost=c(0.01, 0.1, 1, 10, 100)))
# toc()
# 
# 
# #Print optimum value of parameters
# print(OptModelsvm)
# 
# #Plot the performance of SVM Regression model
# plot(OptModelsvm)
# 
# #Find out the best model
# BstModel=OptModelsvm$best.model


# TESTING ---------------------------------------------
# 
# #Predict Y using best model
# pred_svm = predict(BstModel, test16_sparse)
# 
# rmse_svr <- sqrt(mean((pred_svm - output_vector)^2))
# r2_svr <- 1 - (sum((test_vector-pred_svm)^2) / sum((test_vector-mean(pred_svm))^2))

