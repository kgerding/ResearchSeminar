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

house_only16_mv <- fread('./Data/house_only16_mv.csv', drop = 'V1')

colClasses = c(id_parcel = 'numeric',
               num_bathroom = 'numeric',
               num_bedroom = 'numeric',
               area_live_finished = 'numeric',
               flag_tub_or_spa = 'numeric',
               loc_latitude = 'numeric',       
               loc_longitude = 'numeric',
               area_lot = 'numeric',
               factor = 'factor',
               loc_zip  = 'numeric',
               loc_county = 'factor',
               age = 'numeric',
               flag_fireplace = 'numeric',
               num_tax_building = 'numeric',
               num_tax_total = 'numeric',
               num_tax_land = 'numeric',
               num_unit = 'numeric',
               quality_factor = 'factor',
               heating_factor = 'factor',
               prop_living = 'numeric_character',
               build_land_prop = 'numeric_character')

# make conversions
for (i in colnames(house_only16_mv)) {
  if (colClasses[i][[1]] == 'numeric') {
    house_only16_mv[[i]] <- as.numeric(house_only16_mv[[i]])
  } else if (colClasses[[i]] == 'factor') {
    house_only16_mv[[i]] <- as.factor(house_only16_mv[[i]])
  } else if (colClasses[[i]] == 'numeric_character'){
    house_only16_mv[[i]] <- as.numeric(sub(",", ".", house_only16_mv[[i]], fixed = TRUE))
  }
}


# PART 1: DATA PREPROCESSING ###--------------------------------------

# select the dataframe
data_large = na.omit(house_only16_mv)

# use only first 10'000
data = data_large[1:10000,]

##set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# features we want to omit for the model 
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'num_tax_building', 'num_tax_land', 'factor', 'build_land_prop')


# scale data
data$area_live_finished <- log(data$area_live_finished)
data$area_lot <- log(data$area_lot)
data$age <- log(data$age)
data$num_tax_total <- log(data$num_tax_total)


# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define training label = dependent variable
output_vector = as.matrix((train16[,'num_tax_total']))
test_vector = as.matrix((test16[,'num_tax_total']))

#omit variables and convert to numeric again
train16 <- train16 %>% select(-omit)
test16 <- test16 %>% select(-omit)

# Create a sparse matrix
train16_sparse <- data.frame(model.matrix(~ . -1, train16))
test16_sparse <- data.frame(model.matrix(~ . -1, test16))


# MODEL ---------------------

#Regression with SVM
tic()
modelsvm = svm(num_tax_total ~ ., data = train16_sparse,
               type = 'eps-regression',
               kernel = 'linear', 
               scale = FALSE, # don't scale data as we already did
               shrinking = TRUE, # shrink variables
               cross = 5) # cross-validation)
toc()

# simple regression
model_lm = lm(num_tax_total ~ ., data = (train16_sparse))
pred_lm <- predict(model_lm, (test16_sparse))

# Predict using simple regression
rmse_lm <- sqrt(mean((pred_lm - test_vector)^2))
r2_lm <- 1 - (sum((test_vector-pred_lm)^2) / sum((test_vector-mean(pred_lm))^2) )

# Predict using SVM regression
pred_svm = predict(modelsvm, test16_sparse)
rmse_svr <- sqrt(mean((pred_svm - output_vector)^2))
r2_svr <- 1 - (sum((test_vector-pred_svm)^2) / sum((test_vector-mean(pred_svm))^2) )


## Tuning SVR model by varying values of maximum allowable error and cost parameter
#Tune the SVM model
tic()
OptModelsvm=tune(svm, num_tax_total ~ ., data = train16_sparse,
                 type = 'eps-regression', 
                 kernel = 'linear', 
                 scale = FALSE,
                 ranges=list(elsilon=seq(0,1,0.1), 
                             cost=seq(1,101, 10)))
toc()


#Print optimum value of parameters
print(OptModelsvm)

#Plot the performance of SVM Regression model
plot(OptModelsvm)

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
pred_svm = predict(BstModel, test16_sparse)

rmse_svr <- sqrt(mean((pred_svm - output_vector)^2))
r2_svr <- 1 - (sum((test_vector-pred_svm)^2) / sum((test_vector-mean(pred_svm))^2) )

