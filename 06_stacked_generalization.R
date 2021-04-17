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
output_vector = as.matrix(log(train16[,'num_tax_total']))
test_vector = as.matrix(log(test16[,'num_tax_total']))

#omit variables and convert to numeric again
train16 <- train16 %>% dplyr::select(-omit)
test16 <- test16 %>% dplyr::select(-omit)

# Create a sparse matrix
train16_sparse <- data.frame(model.matrix(num_tax_total~ . -1, train16))
test16_sparse <- data.frame(model.matrix(num_tax_total~ . -1, test16))

summary(train16_sparse)


# MODEL --------------------------------------------------

listWrappers() #SL.glmn SL.randomForest, SL.xgboost, SL.svm
set.seed(123)

## you might need to install ranger for Random Forest
# install.packages('ranger')

# The beauty of SuperLearner is that, if a model does not fit well or contribute much, it is just weighted to zero! 
#There is no need to remove it and retrain unless you plan on retraining the model in the future.
layer1.model <- SuperLearner(Y = output_vector,
                             X = train16_sparse,
                             family = gaussian(),
                             method = "method.NNLS", # non-negative least sqaures
                             verbose = TRUE,
                             cvControl = list(V = 2L),
                             SL.library=list("SL.glm", 
                                             "SL.ranger", # Ranger algorithm, which is a faster implementation of the famous Random Forest.
                                             "SL.xgboost", 
                                             "SL.svm", 
                                             "SL.ipredbagg"))

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



