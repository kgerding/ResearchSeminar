# Rsearch Seminar: Bagging  ----------------------------------------------------
# Authors: Tim Graf, Kilian Gerding

### Packages used --------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(doParallel)  
library(foreach)

# for bagging
library(caret)
library(rpart)
library(ipred)
library(pdp)
library(vip)


# RF
library(rpart.plot)
library(randomForest)
library(gbm)
library(MASS)
library(ISLR)
library(adabag)


rm(list=ls())

### PART 1: Prep Data ----------------------------------------------------------

# Reading data
data <- fread('/Users/kiliangerding/Documents/GitHub/ResearchSeminar/Data/house_only16.csv', drop = 'V1')

# define new columns
data$logage <- log(data$age)
data$logarea <- log(data$area_live_finished)

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
               build_land_prop = 'numeric_character',
               logbuild = 'numeric',
               logtotal = 'numeric',
               logage = 'numeric',
               logarea = 'numeric')

# make conversions
for (i in colnames(data)) {
  if (colClasses[i][[1]] == 'numeric') {
    data[[i]] <- as.numeric(data[[i]])
  } else if (colClasses[[i]] == 'factor') {
    data[[i]] <- as.factor(data[[i]])
  } else if (colClasses[[i]] == 'numeric_character'){
    data[[i]] <- as.numeric(sub(",", ".", data[[i]], fixed = TRUE))
  }
}

# omit variables
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county',
          'num_tax_land', 'factor','num_tax_total', 'build_land_prop', 'prop_living')

# Split the data into train and test
data <- data%>% select(-omit)

# select the dataframe
data = na.omit(data)

# use only first 10'000
#data = data[1:10000,]

# Training data
set.seed(123)
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# Split the data into train and test
train16 <- data[train_ind,]
test16 <- data[-train_ind, ]


### PART 2: Prediction of linear Model ----------------------------------------

# linear model specification
hedonic_build <- lm(logbuild ~ num_bathroom + num_bedroom +
                      log(area_live_finished) + 
                      flag_tub_or_spa + log(age) +
                      flag_fireplace ,data = train16)

hedonic_build_fact <- lm(logbuild ~ num_bathroom + num_bedroom +
                           log(area_live_finished) + 
                           flag_tub_or_spa + log(age) + flag_fireplace +
                           factor + num_unit + quality_factor + heating_factor,
                         data = train16)

# prediction with linear models
hedonic_pred = predict(hedonic_build, newdata = test16)
hedonic_fact_pred = predict(hedonic_build_fact, newdata = test16)

# plotting
plot(hedonic_pred, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)

#plotting
plot(hedonic_fact_pred, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)

# calc RSME

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# linear performance
hedonic_rmse = calc_rmse(hedonic_pred, log(test16$num_tax_building))
hedonic_fact_rmse = calc_rmse(hedonic_fact_pred , log(test16$num_tax_building))

hedonic_rmse 
hedonic_fact_rmse

### PART 3: Prediction of bagging Model ----------------------------------------

# set seed for replicability
set.seed(123)

# train simple bagged model w/o CV
price_bagging <- ipred::bagging(
  formula = logbuild ~ num_bathroom + num_bedroom + logarea + logage + flag_tub_or_spa + flag_fireplace,
  data = train16,
  nbagg = 60,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

# show model
price_bagging

# compute forecasts
predict_price <- predict(price_bagging, newdata = test16)

# R2
bagging_r2 = 1- ((sum((test16$logbuild - predict_price)^2))/(sum((test16$logbuild-mean(test16$logbuild))^2)))
bagging_r2

# plotting
plot(predict_price, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)


# assess 10-50 bagged trees
ntree <- 10:150

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  model <- bagging(
    formula = logbuild ~ num_bathroom + num_bedroom + logarea + logage + flag_tub_or_spa + flag_fireplace,
    data    = train16,
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err
}

plot(ntree, rmse, type = 'l', lwd = 2, main = "RMSE vs. Number of N-Trees in a Bagging Model")
abline(v = 25, col = "red", lty = "dashed")

### PART 3: Prediction of bagging Model with Cross Validation ------------------

price_bagging_cv <- bagging.cv(
  formula = logbuild ~ num_bathroom + num_bedroom + logarea +
    logage + flag_tub_or_spa + flag_fireplace,
  data = train16,
  v = 10,
  mfinal = 100,
  control = rpart.control(minsplit = 2, cp = 0),
  par = TRUE
)

# show model
price_bagging_cv

# compute forecasts
predict_price_cv <- predict(price_bagging_cv, newdata = test16)

# plotting
plot(predict_price_cv, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)


# Create a parallel socket cluster
cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

# other model
price_bagging2 <- train(
  formula = log(num_tax_building) ~ num_bathroom + num_bedroom + logarea +
    logage + flag_tub_or_spa + flag_fireplace,
  data = train16,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 100,  
  control = rpart.control(minsplit = 2, cp = 0)
)

stopCluster(cl)

price_bagging2

#plotting
plot(predict_price, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)



#calculate variable importance
VI <- data.frame(var=names(select(test16, -'num_tax_total')), imp=varImp(price_bagging))

#sort variable importance descending
VI_plot <- VI[order(VI$Overall, decreasing=TRUE),]

#visualize variable importance with horizontal bar plot
barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        cex.names = 0.5,
        las = 2,
        horiz=TRUE,
        col='steelblue',
        xlab='Variable Importance')


# Create a parallel socket cluster
cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

# Fit trees in parallel and compute predictions on the test set
predictions <- foreach(
  icount(100), 
  .packages = "rpart", 
  .combine = cbind
) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(train16), replace = TRUE)
  train_boot <- train16[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    logbuild ~ num_bathroom + num_bedroom + logarea + logage + flag_tub_or_spa + flag_fireplace, 
    control = rpart.control(minsplit = 2, cp = 0),
    data = train_boot
  ) 
  
  predict(bagged_tree, newdata = test16)
}

stopCluster(cl)

plot(boston_bag, col = "dodgerblue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()


# Construct partial dependence plots
p1 <- pdp::partial(
  price_bagging, 
  pred.var = 'num_tax_total',
  grid.resolution = 20
) %>% 
  autoplot()

p2 <- pdp::partial(
  price_bagging, 
  pred.var = "Lot_Frontage", 
  grid.resolution = 20
) %>% 
  autoplot()

gridExtra::grid.arrange(p1, p2, nrow = 1)

vip(price_bagging2, num_features = 40)


### PART 4: Prediction of Random Forest with Cross Validation ------------------

price_forest <- randomForest(
  logbuild ~ num_bathroom + num_bedroom + logarea + logage + flag_tub_or_spa + flag_fireplace,
  data = train16,
  mtry = 2, 
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

#plotting
plot(price_forest_pred, log(test16$num_tax_building),
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)
