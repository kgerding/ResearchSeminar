#########################################################
### STACKED GENERALIZATION -----------------------------------
# Authors: Tim Graf, Kilian Gerding
#########################################################

### SETUP ------------------------------------------------------
library(dplyr)
library(Matrix)
library(mlr)
library(parallel)
library(parallelMap) 
library(randomForest)
library(SuperLearner)
library(ranger)
library(data.table)
library(dplyr)
library(tidyverse)
library(tictoc)
library(xgboost)

# start the timer
tic()

setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')
rm(list=ls())

house_only16_mv <- fread('./Data/2016.csv', drop = 'V1')

colClasses = c(num_bathroom = 'numeric',
               num_bedroom = 'numeric',
               area_live_finished = 'numeric_log',
               num_garage = 'numeric', 
               area_garage = 'numeric',
               flag_tub_or_spa = 'numeric',
               loc_latitude = 'numeric',
               loc_longitude = 'numeric',
               area_lot = 'numeric_log',
               loc_city = 'numeric',
               loc_zip  = 'numeric',
               num_pool = 'numeric',
               num_story = 'numeric',
               flag_fireplace = 'numeric',
               num_tax_building = 'numeric_log',
               num_tax_total = 'numeric_log',
               num_tax_property = 'numeric_log',
               age = 'numeric_log')

# make conversions
for (i in colnames(house_only16_mv)) {
  if (colClasses[i][[1]] == 'numeric') {
    house_only16_mv[[i]] <- as.numeric(house_only16_mv[[i]])
  } else if (colClasses[[i]] == 'numeric_log'){
    house_only16_mv[[i]] <- log(as.numeric(house_only16_mv[[i]]))
  }
}



### DATA PREPROCESSING ###--------------------------------------

# select the dataframe
data = (na.omit(house_only16_mv))

# use only first 10'000
#data = (data[1:100000,])

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
output_vector = as.matrix((train16[,'num_tax_building']))
test_vector = as.matrix((test16[,'num_tax_building']))

#omit variables and convert to numeric again
train16_sparse <- train16 %>% dplyr::select(-c(omit, num_tax_building))
test16_sparse <- test16 %>% dplyr::select(-c(omit, num_tax_building))

str(train16)
str(train16_sparse)



# STACKED MODEL  --------------------------------------------------

# listWrappers() #SL.lmn SL.randomForest, SL.xgboost, SL.svm
# 
# ## you might need to install ranger for Random Forest
# # install.packages('ranger')
# 
# # The beauty of SuperLearner is that, if a model does not fit well or contribute much, it is just weighted to zero! 
# # There is no need to remove it and retrain unless you plan on retraining the model in the future.
# # mcSuperLearner is the multiple core version of SuperLearner
# # Note that some algorithms do not just require a data frame, but would require a model matrix saved as a data frame. An example is the nnet algorithm


### CREATE INDIVIDUAL LEARNERS ### ------------------------------------------------

# Load coefficient from other models
load("./Models/params_xgb.RData")
load("./Models/xgb_best_iteration.RData")

# Create learners 
# the parameters are transfered from what has been tuned in the other files
learner_ranger <- create.Learner("SL.ranger", params=list(num.trees=1000, 
                                                          mtry=3, 
                                                          sample.fraction = 0.5, 
                                                          verbose = TRUE))
learner_xgb <- create.Learner("SL.xgboost", params=list(ntrees = xgb_best_iteration, 
                                                        objective = params_xgb$objective,
                                                        max_depth = params_xgb$max_depth, 
                                                        eta = params_xgb$eta, 
                                                        booster = params_xgb$booster,
                                                        colsample_bytree = params_xgb$colsample_bytree,
                                                        gamma = params_xgb$gamma,
                                                        subsample = params_xgb$subsample,
                                                        min_child_weight = params_xgb$min_child_weight,
                                                        verbose = TRUE))
learner_bagg <- create.Learner("SL.ipredbagg", params=list(nbagg=100, 
                                                           tree_depth = 15)) 


# Parallelization
options(mc.cores = detectCores())
getOption("mc.cores")

# cluster = parallel::makeCluster(detectCores()-2)
# parallel::clusterEvalQ(cluster, library(SuperLearner))
# parallel::clusterExport(cluster, c(learner_ranger$names, learner_xgb$names,learner_bagg$names) )
# 

# Train the learner
layer.model4 <- SuperLearner(Y = train16$num_tax_building, 
                               X = data.frame(train16_sparse),
                               newX = data.frame(test16_sparse),
                               family = gaussian(),
                               method = "method.NNLS", # non-negative least squares
                               verbose = TRUE,
                               #cluster = cluster,
                               cvControl = list(V = 5), # the number of folds
                               SL.library=list("SL.lm", 
                                               learner_ranger$names, 
                                               learner_xgb$names, 
                                               learner_bagg$names))

# check the final coefficients
layer.model4$cvRisk
layer.model4$coef # coefficients for the super learner
head(layer.model4$Z) #the z-matrix: cross-validated predicted values for each algorithm in SL. library
head(layer.model4$library.predict) # the prediction for the z-matrix
head(layer.model4$SL.predict) # the final prediction
#head(predict.SuperLearner(layer.model4, newdata = test16_sparse)$pred) # the final prediction


# # cross-validation to see which algorithms perfom best
# cv.model4 <- CV.SuperLearner(Y = train16$num_tax_building,
#                              X = data.frame(train16_sparse),
#                              family = gaussian(),
#                              verbose = TRUE,
#                              parallel = 'multicore',
#                              method = "method.NNLS", # non-negative least sqaures
#                              V = 5,
#                              SL.library=list("SL.lm",
#                                              learner_ranger$names,
#                                              learner_xgb$names,
#                                              learner_bagg$names))


### TESTING THE MODEL ###--------------------------------------------

# predict
pred_stacked_test <- layer.model4$SL.predict
pred_stacked_train <- (predict.SuperLearner(layer.model4, newdata = train16_sparse))
pred_stacked_train <- pred_stacked_train$pred

# metrics train
rmse_stacked_train <- sqrt(mean((pred_stacked_train - output_vector)^2))
r2_stacked_train <- 1 - ( sum((output_vector-pred_stacked_train)^2) / sum((output_vector-mean(output_vector))^2) )
adj_r2_stacked_train <- 1 - ((1 - r2_stacked_train) * (nrow(output_vector) - 1)) / (nrow(output_vector) - ncol(output_vector) - 1)

# metrics test
rmse_stacked_test <- sqrt(mean((pred_stacked_test - test_vector)^2))
r2_stacked_test <- 1 - ( sum((test_vector-pred_stacked_test)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_stacked_test <- 1 - ((1 - r2_stacked_test) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)

# combining results
results_stacked_train <- rbind(rmse_stacked_train, r2_stacked_train, adj_r2_stacked_train)
results_stacked_test <- rbind(rmse_stacked_test, r2_stacked_test, adj_r2_stacked_test)
results_stacked <- data.frame(cbind(results_stacked_train, results_stacked_test))
colnames(results_stacked) <- c("train_stacked", "test_stacked")
rownames(results_stacked) <- c("RMSE", "R2", "ADJ_R2")

# calculating errors
errors_stacked <- pred_stacked_test - test_vector


### COMPARE RESULTS TO SIMPLE LM  ### --------------------------------------------

train16_sparse_lm <- cbind(train16_sparse, output_vector)
test16_sparse_lm <- cbind(test16_sparse, test_vector)

# learn model
model_lm = lm(num_tax_building ~ ., data = data.frame(train16_sparse_lm))

# predict
pred_lm <- (predict(model_lm, (test16_sparse_lm)))

# Predict using simple regression
rmse_lm <- sqrt(mean((pred_lm - test_vector)^2))
r2_lm <- 1 - (sum((test_vector-pred_lm)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_lm <- 1 - ((1 - r2_lm) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)


### PLOTS ###--------------------------------------------------

# merge dataframes
merged_df <- data.frame(cbind(pred_stacked_test, test_vector, test16_sparse)) #by 0 merges based on index
colnames(merged_df)[1] <- "pred_stacked_test"
merged_df <- merged_df[order(merged_df$num_tax_building),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_stacked <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = pred_stacked_test, color = 'predicted')) +
  geom_point(aes(y = num_tax_building, color = 'actual')) +
  ggtitle('Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_stacked

# Plot CV-Model
plot(cv.model4)



### SAVE PLOTS AND DATAFRAMES ### --------------------------------------------

# save actual vs. predicted
ggsave('plot_stacked.png', path = './Plots/', plot = plot_stacked, device = 'png')

# save cv model plot
ggsave('plot_stacked_cv.png', path = './Plots/', plot = cv.model4, device = 'png')

# save comparison_xgb
save(results_stacked,file="./Models/results_stacked.Rdata")

# save errors of predictions on test
save(errors_stacked,file="./Models/errors_stacked.Rdata")

# stop the timer
toc()


