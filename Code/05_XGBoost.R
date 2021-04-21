#########################################################
### XGBOOST----------------------------------
# Authors: Tim Graf, Kilian Gerding
#########################################################

"note: Xgboost manages only numeric vectors.
For many machine learning algorithms, using correlated features is not a good idea. 
It may sometimes make prediction less accurate, and most of the time make interpretation of the model 
almost impossible. GLM, for instance, assumes that the features are uncorrelated.

Fortunately, decision tree algorithms (including boosted trees) are very robust to these features. 
Therefore we have nothing to do to manage this situation.
"

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



### PART 1: DATA PREPROCESSING ###--------------------------------------

# select the dataframe
data = (na.omit(house_only16_mv))

# use only first 10'000
data = (data[1:100000,])

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
train16 <- train16 %>% dplyr::select(-omit)
test16 <- test16 %>% dplyr::select(-omit)

# convert categorical factor into dummy variables using one-hot encoding
sparse_matrix_train <- sparse.model.matrix((num_tax_building)~.-1, data = train16)
sparse_matrix_test <- sparse.model.matrix((num_tax_building)~.-1, data = test16)

# check the dimnames crated by the one-hot encoding
sparse_matrix_train@Dimnames[[2]]

# Create a dense matrix
dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = output_vector)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label=test_vector)



### PART 2: XGBOOST Training ###  -----------------------------


# FIND OPTIMIZED PARAMETERS # 


#set.seed(123)

fact_col <- colnames(train16)[sapply(train16,is.character)]

for(i in fact_col) set(train16,j=i,value = factor(train16[[i]]))
for (i in fact_col) set(test16,j=i,value = factor(test16[[i]]))

# create tasks for learner
traintask <- makeRegrTask(data = data.frame(train16), target = 'num_tax_building')
testtask <- makeRegrTask(data = data.frame(test16), target = 'num_tax_building')

# create dummy features, as classif.xgboost does not support factors
traintask <- createDummyFeatures(obj = traintask)
testtask <- createDummyFeatures(obj = traintask)

# create learner
# fix number of rounds and eta 
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror",
                     eval_metric="rmse", 
                     nrounds=100L, 
                     eta = 0.1)

# set parameter space
# for computational reasons we only optimize the most important variables with are the booster type and the max depth per tree
params_xgb <- makeParamSet(makeDiscreteParam("booster", values = c("gbtree", "dart")), # gbtree and dart - use tree-based models, while glinear uses linear models
                           makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                           makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                           makeNumericParam("subsample",lower = 0.2,upper = 1), 
                           makeNumericParam("colsample_bytree",lower = 0.1,upper = 1), 
                           makeDiscreteParam("eta", values = c(0.05, 0.1, 0.2)),
                           makeDiscreteParam("gamma", values = c(0, 0.2, 0.5, 0.7))
)

# set resampling strategy
# If you have many classes for a classification type predictive modeling problem or the classes are imbalanced 
# (there are a lot more instances for one class than another), it can be a good idea to create stratified folds when performing cross validation.
rdesc <- makeResampleDesc("CV",stratify = F, iters=5L)

# search strategy
# instead of a grid search we use a random search strategy to find the best parameters. 
ctrl <- makeTuneControlRandom(maxit = 10L) #maxit is the number of iterations for random search

# set parallel backend
parallelStartSocket(cpus = detectCores(), level = "mlr.tuneParams")


# parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     par.set = params_xgb, 
                     control = ctrl, 
                     show.info = TRUE)

parallelStop()

# print the optimal parameters
mytune



### MODEL 2: XGBOOST WITH OPTIMIZED PARAMETERS ###  -----------------------------

# take the parameters of mytune
params_xgb <- list(booster = mytune$x$booster, 
                   objective = "reg:squarederror",
                   eta=mytune$x$eta, # learning rate, usually between 0 and 1. makes the model more robust by shrinking the weights on each step
                   gamma=mytune$x$gamma, # regularization (prevents overfitting), higher means more penalty for large coef. makes the algo more conservative
                   subsample= mytune$x$subsample, # fraction of observations taken to make each tree. the lower the more conservative and more underfitting, less overfitting. 
                   max_depth = mytune$x$max_depth, # max depth of trees, the more deep the more complex and overfitting
                   min_child_weight = mytune$x$min_child_weight, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                   colsample_bytree = mytune$x$colsample_bytree) # number of variables per tree, typically between 0.5 - 0.9

# using cross-validation to find optimal nrounds parameter
xgbcv <- xgb.cv(params = params_xgb,
                data = dtrain, 
                nrounds = 1000L, 
                nfold = 5,
                showsd = T, # whether to show standard deviation of cv
                stratified = F, 
                print_every_n = 1, 
                nthread = detectCores(),
                early_stopping_rounds = 50, # stop if we don't see much improvement
                maximize = F, # should the metric be maximized?
                verbose = 2)

# Result of best iteration
xgb_best_iteration <- xgbcv$best_iteration


# training with optimized nrounds
xgb2 <- xgb.train(params = params_xgb, 
                  data = dtrain, 
                  nrounds = xgb_best_iteration, 
                  watchlist = list(test = dtest, train = dtrain), 
                  maximize = F, 
                  eval_metric = "rmse"
)


### TESTING THE MODEL ###--------------------------------------------

# predict
xgb2_pred_train <- predict(xgb2, dtrain)
xgb2_pred_test <- predict(xgb2, dtest)

# metrics for train
rmse_xgb2_train <- sqrt(mean((xgb2_pred_train - output_vector)^2))
r2_xgb2_train <- 1 - ( sum((output_vector-xgb2_pred_train)^2) / sum((output_vector-mean(output_vector))^2) )
adj_r2_xgb_train <- 1 - ((1 - r2_xgb2_train) * (nrow(output_vector) - 1)) / (nrow(output_vector) - ncol(output_vector) - 1)

# metrics for test
rmse_xgb2_test <- sqrt(mean((xgb2_pred_test - test_vector)^2))
r2_xgb2_test <- 1 - ( sum((test_vector-xgb2_pred_test)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_xgb_test <- 1 - ((1 - r2_xgb2_test) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)

# combining results
results_xgb_train <- rbind(rmse_xgb2_train, r2_xgb2_train, adj_r2_xgb_train)
results_xgb_test <- rbind(rmse_xgb2_test, r2_xgb2_test, adj_r2_xgb_test)
results_xgb <- data.frame(cbind(results_xgb_train, results_xgb_test))
colnames(results_xgb) <- c("train_xgb", "test_xgb")
rownames(results_xgb) <- c("RMSE", "R2", "ADJ_R2")

errors_xgb <- xgb2_pred_test - test_vector

### COMPARE RESULTS TO SIMPLE LM  ### --------------------------------------------
train16_sparse <- data.frame(model.matrix(~ . -1, train16))
test16_sparse <- data.frame(model.matrix(~ . -1, test16))

# train and predict 
model_lm = lm(num_tax_building ~ ., data = data.frame(train16_sparse))
pred_lm <- log(predict(model_lm, (test16_sparse)))

# Predict using simple regression
rmse_lm <- sqrt(mean((pred_lm - test_vector)^2))
r2_lm <- 1 - (sum((test_vector-pred_lm)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_lm <- 1 - ((1 - r2_lm) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)



# PLOTS --------------------------------------------------

# plot an example tree of all
xgb.plot.tree(feature_names = names(dtrain), 
              model = xgb2, 
              trees = 1)

# Plot importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb2)
xgb_importance <- xgb.plot.importance(importance_matrix = importance, top_n = 15)
plot_xgb_importance <- xgb_importance %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  ggplot(aes(x=Feature, y=Importance)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle('Feature Importance Plot for XG-Boost')
plot_xgb_importance

# define top 3 relevant variables
variable1 = xgb_importance$Feature[1]
variable2 = xgb_importance$Feature[2]
variable3 = xgb_importance$Feature[3]

# merge dataframes
merged_df <- data.frame(cbind(xgb2_pred_test, test16_sparse)) #by 0 merges based on index
merged_df <- merged_df[order(merged_df$num_tax_building),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_xgb <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = xgb2_pred_test, color = 'predicted')) +
  geom_point(aes(y = num_tax_building, color = 'actual')) +
  ggtitle('Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_xgb

# Plot most Top 1 variable vs. actual 
plot_v1 <- ggplot(data = merged_df) +
  geom_point(aes(x = !!ensym(variable1), y = num_tax_building)) +
  ggtitle(paste0('Log(num_tax_building) vs. ', variable1))
plot_v1

# Plot most Top 2 variable vs. actual 
plot_v2 <- ggplot(data = merged_df) +
  geom_point(aes(x = !!ensym(variable2), y = num_tax_building)) +
  ggtitle(paste0('Log(num_tax_building) vs. ', variable2))
plot_v2

# Plot most Top 3 variable vs. actual 
plot_v3 <- ggplot(data = merged_df) +
  geom_point(aes(x = !!ensym(variable3), y = num_tax_building)) +
  ggtitle(paste0('Log(num_tax_building) vs. ', variable3))
plot_v3


# SAVE MODELS AND PLOTS -----------------------------

# save plot
ggsave('plot_xgb_v1.png', path = './Plots/', plot = plot_v1, device = 'png')
ggsave('plot_xgb_v2.png', path = './Plots/', plot = plot_v2, device = 'png')
ggsave('plot_xgb_v3.png', path = './Plots/', plot = plot_v3, device = 'png')
ggsave('plot_xgb.png', path = './Plots/', plot = plot_xgb, device = 'png')
ggsave('plot_xgb_importance.png', path = './Plots/', plot = plot_xgb_importance, device = 'png')

# save model to local file
xgb.save(xgb2, "./Models/xgboost.model")

# save results
save(results_xgb,file="./Models/results_xgboost.RData")

# save errors
save(errors_xgb, file = "./Models/errors_xgb.RData")

# save parameters
save(params_xgb, file = "./Models/params_xgb.RData")
save(xgb_best_iteration, file = "./Models/xgb_best_iteration.RData")


toc()
