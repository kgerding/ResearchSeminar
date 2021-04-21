#########################################################
### RANDOM FOREST -----------------------------------
# Authors: Tim Graf, Kilian Gerding
#########################################################

### Packages used --------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(doParallel)  
library(foreach)
library(data.table)
library(forcats)


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
library(ranger)
library(e1071)
library(dplyr)


# for parallel
library(doParallel)
library(tictoc)


rm(list=ls())
tic()

### PART 1: Prep Data ----------------------------------------------------------

# Reading data
setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')

data <- fread('./Data/2016.csv', drop = 'V1')

# for computational reasons
data <- data[1:10000]


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


# set up parallel
cl<-makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)

# # Random Search (this is computationally intense)
# bestMtry <- tuneRF(x = train16, y = output_vector, stepFactor = 1.5, improve = 1e-5, ntree = 100)
# print(bestMtry)

# train the model
price_forest <- ranger(model, 
                       data = train16,
                       num.trees = 100, 
                       mtry = round(11/3), 
                       
                       # permutation importance. The basic idea is to consider a variable important if it has a positive effect on the 
                       #prediction accuracy (classification), or MSE (regression)
                       importance = 'permutation', 
                       
                       num.threads = detectCores(),
                       verbose = TRUE)
stopCluster(cl)


### PLOTTING ### ---------------------------------------

# variable importance
varImp <- data.frame(price_forest$variable.importance)
varImp['variable'] <- rownames(varImp)
colnames(varImp) <- c('importance', 'variable')
rownames(varImp) <- NULL

# Plot importance
plot_rf_importance <- varImp %>%
  mutate(variable = fct_reorder(variable, importance)) %>%
  ggplot(aes(x=variable, y=importance)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle('Feature Importance Plot for Random Forest')
plot_rf_importance

price_forest_pred = predict(price_forest, data = test16)
price_forest_pred <- price_forest_pred$predictions
price_forest_pred_train = predict(price_forest, data = train16)
price_forest_pred_train <- price_forest_pred_train$predictions

#plotting predicted vs. actual 
plot(price_forest_pred, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)

### PART 3: TESTING THE MODEL ###--------------------------------------------

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

### PLOTS ###--------------------------------------------

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_rf <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = price_forest_pred, color = 'predicted')) +
  geom_point(aes(y = logbuild, color = 'actual')) +
  ggtitle('Random Forest: Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_rf


### SAVE PLOTS AND DATAFRAMES ### --------------------------------------------

# save actual vs. predicted
ggsave('plot_rf.png', path = './Plots/', plot = plot_rf, device = 'png')

# rf importance
ggsave('plot_rf_importance.png', path = './Plots/', plot = plot_rf_importance, device = 'png')

# save comparison_xgb
save(results_rf,file="./Models/results_rf.Rdata")

# save errors of predictions on test
save(errors_rf,file="./Models/errors_rf.Rdata")

# stop the timer
toc()

