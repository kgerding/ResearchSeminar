#########################################################
### Support Vector Machines -----------------------------------
#########################################################

library(Matrix)
library(mlr)
library(data.table)
library(dplyr)
library(tidyverse)

rm(list=ls())

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

str(house_only16_mv)


### PART 1: DATA PREPROCESSING ###--------------------------------------

# select the dataframe
data = na.omit(house_only16_mv)

# use only first 10'000
data = data[1:10000,]

##set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# features we want to omit for the model 
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'num_tax_building', 'num_tax_land', 'factor')

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define training label = dependent variable
output_vector = as.matrix(log(train16[,'num_tax_total']))
test_vector = as.matrix(log(test16[,'num_tax_total']))

#omit variables and convert to numeric again
train16 <- train16 %>% select(-omit)
#train16 <- data.frame(sapply(train16, as.numeric))
test16 <- test16 %>% select(-omit)
#test16 <- data.frame(sapply(test16, as.numeric))

# convert categorical factor into dummy variables using one-hot encoding
sparse_matrix_train <- sparse.model.matrix(log(num_tax_total)~.-1, data = train16)
sparse_matrix_test <- sparse.model.matrix(log(num_tax_total)~.-1, data = test16)

# check the dimnames crated by the one-hot encoding
sparse_matrix_train@Dimnames[[2]]

# Create a dense matrix
dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = output_vector)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label=test_vector)
