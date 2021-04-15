#########################################################
### Deep Learning -----------------------------------
#########################################################

library(Matrix)
library(mlr)
library(data.table)
library(dplyr)
library(tidyverse)

library(keras)
library(tfdatasets)
library(dplyr)

library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)


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

# select the dataframe
data = na.omit(house_only16_mv)

# use only first 10'000
data = data[1:10000,]

##set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# features we want to omit for the model 
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'num_tax_building', 'num_tax_land', 'factor', 'build_land_prop', 'prop_living')

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define training label = dependent variable
output_vector = as.matrix((train16[,'num_tax_total']))
test_vector = as.matrix((test16[,'num_tax_total']))

#omit variables and convert to numeric again
train16 <- train16 %>% select(-omit)
test16 <- test16 %>% select(-omit)

# # convert categorical factor into dummy variables using one-hot encoding
# sparse_matrix_train <- sparse.model.matrix(log(num_tax_total)~.-1, data = train16)
# sparse_matrix_test <- sparse.model.matrix(log(num_tax_total)~.-1, data = test16)
# 
# # check the dimnames crated by the one-hot encoding
# sparse_matrix_train@Dimnames[[2]]

# Create a dense matrix
subset1 <- subset(train16, select = -num_tax_total)
train16 <- as.matrix(model.matrix(~ . -1, subset1))

subset2 <- subset(test16, select = -num_tax_total)
test16 <- as.matrix(model.matrix(~ . -1, subset2))

# Scale features
train16 <- scale(train16)
train16 <- train16[,!(colSums(is.na(train16)) > 0)]
train16 <- as.matrix(train16)

test16 <- scale(test16)
test16 <- train16_with[,!(colSums(is.na(test16)) > 0)]
test16 <- as.matrix(test16)


#train16_without <- train16 %>% select(-c(flag_tub_or_spa, flag_fireplace, heating_factor, quality_factor))

# # normalize features
# train16_without <- keras::normalize(train16_with[,2:34])
# train16_without <- train16_with %>% mutate_at(c("num_bathroom", "num_bedroom", "area_live_finished", "area_lot", "age", "num_unit"), ~(scale(.) %>% as.vector))
# 
# rec_obj <- recipe(num_tax_total ~ ., data = train16) %>%
#   step_log(area_live_finished, area_lot) %>%
#   step_dummy(all_nominal(), -all_outcomes()) %>%
#   step_center(all_predictors(), -all_outcomes()) %>%
#   step_scale(all_predictors(), -all_outcomes()) %>%
#   prep(data = train16)
# 
# x_train_tbl <- bake(rec_obj, newdata = train16, composition = 'data.frame') %>% select(-num_tax_total)
# x_test_tbl  <- bake(rec_obj, newdata = test16) %>% select(-num_tax_total)

# #normalize features
# spec1 <- feature_spec(train16_without, num_tax_total ~ . ) %>% 
#   step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
#   fit()


# spec1 <- feature_spec(train16, num_tax_total ~ . ) %>%
#   step_numeric_column(num_bathroom,
#                       num_bedroom,
#                       area_live_finished,
#                       area_lot,
#                       age,
#                       num_unit,
#                       normalizer_fn = scaler_standard()) %>%
#   step_numeric_column(flag_tub_or_spa, flag_fireplace) %>%
#   step_categorical_column_with_vocabulary_list(quality_factor, heating_factor) %>%
#   step_indicator_column(quality_factor, heating_factor) %>% 
#   fit()
# 
# spec1
# str(spec1$dense_features())


input1 <- layer_input_from_dataset(train16_without %>% select(-num_tax_total))

output1 <- input1 %>% 
  layer_dense_features(dense_features(spec1)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) 

model <- keras_model(input1, output1)

#summary(model)


# build model
model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )


# define function
build_model1 <- function() {
  input <- layer_input_from_dataset(train16_without %>% select(-num_tax_total))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec1)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input1, output1)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}

# make a function to print "." as progress bar for each n epochs
n = 50
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% n == 0) cat("\n")
    cat(".")
  }
)    

# Run function
model1 <- build_model1()

history <- model1 %>% fit(
  x = train16_without %>% select(-num_tax_total),
  y = train16_without$num_tax_total,
  epochs = 50,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

history

plot(history)
#str(train16_with)


# # Let's see how the model performs on the test data set
# c(loss, mae) %<-% (model %>% evaluate(test16_with %>% select(-num_tax_total), test16_with$num_tax_total, verbose = 0))
# 
# paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

# -TRIAL WITH BOSTEN DATA HOUSING SET---------------------------
#https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/

# 
# 
# boston_housing <- dataset_boston_housing()
# 
# c(train_data, train_labels) %<-% boston_housing$train
# c(test_data, test_labels) %<-% boston_housing$test
# 
# 
# column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE',
#                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
# 
# train_df <- train_data %>%
#  as_tibble(.name_repair = "minimal") %>%
#  setNames(column_names) %>%
#  mutate(label = train_labels)
# 
# test_df <- test_data %>%
#  as_tibble(.name_repair = "minimal") %>%
#  setNames(column_names) %>%
#  mutate(label = test_labels)
# 
# #normalize features
# spec <- feature_spec(train_df, label ~ . ) %>%
#  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>%
#  fit()
# 
# spec
# 
# #
# # layer <- layer_dense_features(
# #  feature_columns = dense_features(spec),
# #  dtype = tf$float32
# # )
# # layer(train_df)
# 
# input <- layer_input_from_dataset(train_df %>% select(-label))
# 
# output <- input %>%
#  layer_dense_features(dense_features(spec)) %>%
#  layer_dense(units = 64, activation = "relu") %>%
#  layer_dense(units = 64, activation = "relu") %>%
#  layer_dense(units = 1)
# 
# model <- keras_model(input, output)
# 
# summary(model)
# 
# #build model
# model %>%
#  compile(
#    loss = "mse",
#    optimizer = optimizer_rmsprop(),
#    metrics = list("mean_absolute_error")
#  )
# 
# 
# build_model <- function() {
#  input <- layer_input_from_dataset(train_df %>% select(-label))
# 
#  output <- input %>%
#    layer_dense_features(dense_features(spec)) %>%
#    layer_dense(units = 64, activation = "relu") %>%
#    layer_dense(units = 64, activation = "relu") %>%
#    layer_dense(units = 1)
# 
#  model <- keras_model(input, output)
# 
#  model %>%
#    compile(
#      loss = "mse",
#      optimizer = optimizer_rmsprop(),
#      metrics = list("mean_absolute_error")
#    )
# 
#  model
# }
# 
# 
# # Display training progress by printing a single dot for each completed epoch.
# print_dot_callback <- callback_lambda(
# on_epoch_end = function(epoch, logs) {
#   if (epoch %% 80 == 0) cat("\n")
#   cat(".")
# }
# )
# 
# model <- build_model()
# 
# history <- model %>% fit(
#   x = train_df %>% select(-label),
#   y = train_df$label,
#   epochs = 500,
#   validation_split = 0.2,
#   verbose = 0,
#   callbacks = list(print_dot_callback)
#   )
# 
# plot(history)
