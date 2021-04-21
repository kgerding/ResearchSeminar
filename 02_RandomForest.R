# Rsearch Seminar: Random Forest  ----------------------------------------------
# Authors: Tim Graf, Kilian Gerding

### Packages used --------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(doParallel)  
library(foreach)
library(data.table)

# for bagging
library(caret)
library(rpart)
library(ipred)
library(pdp)
library(vip)

# RF
library(ranger)
library(rpart.plot)
library(randomForest)
library(gbm)
library(MASS)
library(ISLR)
library(tidymodels)
library(baguette)
library(dials)
library(workflows)
library(tune)
library(recipes)
library(rsample)
library(yardstick)


rm(list=ls())

### PART 1: Prep Data ----------------------------------------------------------

# Reading data
data <- fread('/Users/kiliangerding/Documents/GitHub/ResearchSeminar/Data/2016.csv', drop = 'V1')

# define logs for simplicity
data$logbuild <- log(data$num_tax_building)
data$logarea <- log(data$area_live_finished)
data$logage <- log(data$age)
data$loglot <- log(data$area_lot)
data$loggarage <- ifelse(data$num_garage > 0,log(data$num_garage),0)

# define model
model <- logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa

# Training data
set.seed(123)

# Split the data into train and test
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]

# define output and test vector
output_vector <- matrix(train16$logbuild)
test_vector <- matrix(test16$logbuild)

### PART 2: Prediction of Random Forest with Cross Validation ------------------

# random tuning

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=5, search="random")

set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_random <- train(model, data=train16, method="rf", metric='RMSE', tuneGrid=tunegrid, trControl=control)
print(rf_random)
plot(rf_random)

price_forest <- randomForest(
  model,
  data = train16,
  mtry = 3, 
  importance = TRUE,
  ntrees = 500
)

# show results
price_forest

# plot RSME
plot(price_forest, col = "blue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()

# variable importance
varImpPlot(price_forest, type = 1, main = 'Relative Importance of Variables')

price_forest_pred = predict(price_forest, newdata = test16)
price_forest_pred_train = predict(price_forest, newdata = train16)

#plotting
plot(price_forest_pred, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)

# metrics for train
rmse_rf_train <- sqrt(mean((price_forest_pred_train - output_vector)^2))
r2_rf_train <- 1 - ( sum((output_vector-price_forest_pred_train)^2) / sum((output_vector-mean(output_vector))^2) )
adj_r2_rf_train <- 1 - ((1 - r2_rf_train) * (nrow(output_vector) - 1)) / (nrow(output_vector) - ncol(output_vector) - 1)

# metrics for test
rmse_rf_test <- sqrt(mean((price_forest_pred - test_vector)^2))
r2_rf_test <- 1 - ( sum((test_vector-price_forest_pred)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_rf_test <- 1 - ((1 - r2_rf_test) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)

# combining results
results_rf_train <- rbind(rmse_rf_train, r2_rf_train, adj_r2_rf_train)
results_rf_test <- rbind(rmse_rf_test, r2_rf_test, adj_r2_rf_test)
results_rf <- data.frame(cbind(results_rf_train, results_rf_test))
colnames(results_rf) <- c("train_rf", "test_rf")
rownames(results_rf) <- c("RMSE", "R2", "ADJ_R2")

errors_rf <- price_forest_pred - test_vector

# merge dataframes
merged_df <- data.frame(cbind(price_forest_pred, test16)) #by 0 merges based on index
merged_df <- merged_df[order(merged_df$logbuild),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_rf <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = price_forest_pred, color = 'predicted')) +
  geom_point(aes(y = logbuild, color = 'actual')) +
  ggtitle('Hedonic Regression: Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_rf
