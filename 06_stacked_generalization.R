#########################################################
### Stacked Generalization -----------------------------------
#########################################################

library(data.table)
library(dplyr)
library(tidyverse)
library(e1071) # for SVM
library(tictoc) # to measure time elapsed
library(SuperLearner)
library(MASS)


# SETUP ------------------------------------------------------
library(xgboost)
library(Matrix)
library(mlr)
library(parallel)
library(parallelMap) 
library(randomForest)
library(data.table)
library(dplyr)
library(tidyverse)
library(tictoc)

tic()

setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')
rm(list=ls())

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
omit <- c('loc_latitude', 'loc_longitude', 'loc_zip', 'loc_city', 'num_tax_total', 'num_tax_property')

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define training label = dependent variable
output_vector = as.matrix(log(train16[,'num_tax_building']))
test_vector = as.matrix(log(test16[,'num_tax_building']))

#omit variables and convert to numeric again
train16_sparse <- train16 %>% dplyr::select(-c(omit, num_tax_building))
test16_sparse <- test16 %>% dplyr::select(-c(omit, num_tax_building))

str(train16)


# MODEL --------------------------------------------------

listWrappers() #SL.glmn SL.randomForest, SL.xgboost, SL.svm
set.seed(123)

## you might need to install ranger for Random Forest
# install.packages('ranger')

# The beauty of SuperLearner is that, if a model does not fit well or contribute much, it is just weighted to zero! 
#There is no need to remove it and retrain unless you plan on retraining the model in the future.
layer1.model <- SuperLearner(Y = output_vector, 
                             X = train16,
                             family = gaussian(),
                             method = "method.NNLS", # non-negative least sqaures
                             verbose = TRUE,
                             cvControl = list(V = 2L),
                             SL.library=list(#"SL.glm", 
                                             "SL.ranger")) # Ranger algorithm, which is a faster implementation of the famous Random Forest.
                                             #"SL.xgboost", 
                                            # "SL.svm", 
                                             #"SL.ipredbagg"))

summary(layer1.model)

# choosing the right algorithms using cross-validation
scv.model <- CV.SuperLearner(Y = output_vector,
                             X = train16_sparse,
                             family = gaussian(),
                             verbose = TRUE,
                             parallel = 'multicore',
                             method = "method.NNLS", # non-negative least sqaures
                             V = 5,
                             SL.library=list("SL.glm", 
                                             "SL.ranger", 
                                             "SL.xgboost", 
                                             "SL.svm", 
                                             "SL.ipredbag"))
summary(cv.model)
plot(cv.model)


# specifying the model with tuning
SL.rf.tune <- function(...){
  SL.ranger(..., num.trees=1000, mtry=2)
}

SL.i.tune <- function(...){
  SL.ipredbagg(..., nbagg=250)
}

# making a prediction with the stacked model
predictions <- predict.SuperLearner(layer1.model, newdata = test16_sparse)
head(predictions$pred) # overall ensemble prediction
head(predictions$library.predict) # individual library predictions



