#########################################################
### Deep Learning  Try 2 -----------------------------------
#########################################################


# https://www.roelpeters.be/using-keras-in-r-training-a-model/

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
train16_y = as.matrix((train16[,'num_tax_total']))
test16_y = as.matrix((test16[,'num_tax_total']))

#omit variables and convert to numeric again
train16_x <- train16 %>% select(-omit)
test16_x <- test16 %>% select(-omit)

# Create a dense matrix
subset1 <- subset(train16_x, select = -num_tax_total)
train16_x <- as.matrix(model.matrix(~ . -1, subset1))

subset2 <- subset(test16_x , select = -num_tax_total)
test16_x <- as.matrix(model.matrix(~ . -1, subset2))

# Scale features
train16_x <- scale(train16_x) 
train16_x <- as.matrix(train16_x[,!(colSums(is.na(train16_x)) > 0)])

test16_x <- scale(test16_x)
test16_x <- as.matrix(test16_x[,!(colSums(is.na(test16_x)) > 0)])

summary(train16_x)

## omit all factor variables to test if the model gets any better
# train16_x <- train16_x[, c(1:7)]
# test16_x <- test16_x[, c(1:7)]



# MODEL -------------------------------------

# check https://blogs.rstudio.com/ai/posts/2018-01-11-keras-customer-churn/

# Set up model
build_model <- function() {
  model <- keras_model_sequential() 
  model %>% 
    
    # these are hidden layers which capture non-linear relationships
    layer_dense(units = 16, 
                kernel_initializer = "uniform",
                input_shape = ncol(train16_x), # first layers needs the number of columns of the training data
                kernel_regularizer = regularizer_l2(l = 0.001)) %>% # If 𝐿2  regularization (aka weight decay) or 𝐿1 regularization is set too large, so the weights can't move.
    
    # layer_activation_relu() %>% # (2) Separate ReLU layer
  
    # second layer
    layer_dense(units = 128, 
                kernel_initializer = "uniform",
                activation = 'relu') %>% 
    
    # dropout layers are here to mitigate overfitting. the number refers to weights being dropped below (e.g. 10%)
    layer_dropout(0.1) %>% 
    
    # third layyer
    layer_dense(units = 64, 
                kernel_initializer = "uniform",
                activation = 'relu') %>%
    
    # output layer 
    layer_dense(units = 1)
  
  # compile 
  model %>% compile(
    loss = "mse",
    optimizer = 'adam', # this is the optimizer algorithm
    metrics = list("mean_squared_error")
  )
  model
}

model <- build_model()
model %>% summary()


# Function to see live progress
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

# Specify if the model should stop early
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 100)
epochs <- 100

# Fit the model and store training stats
history <- model %>% fit(
  train16_x,
  train16_y,
  epochs = epochs,
  #batch_size = 50, # sets the number samples per gradient update within each epoch 
  validation_split = 0.0, # it will split training data into validation and training to avoid overfitting
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

print(history)
plot(history)

# Test our model on the test data
y_test_pred <- model %>% predict(test16_x)
rmse_test <- mean(sum(y_test_pred - test16_y)^2)

ggplot(data.table('test' = c(test16_y),'pred' = c(y_test_pred)), aes(x=test, y=pred)) + 
  geom_point() + 
  geom_smooth(method='lm')


# why doesn't it learn?
# https://stats.stackexchange.com/questions/352036/what-should-i-do-when-my-neural-network-doesnt-learn

