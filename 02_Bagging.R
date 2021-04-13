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


rm(list=ls())

# Reading data
data <- house_only16[1:100000,]

# Training data
smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# omit variables
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county',
          'num_tax_land', 'factor','num_tax_total', 'build_land_prop', 'prop_living')

# Split the data into train and test
train16 <- data[train_ind,]
train16 <- train16 %>% select(-omit)

test16 <- data[-train_ind, ]
test16 <- test16 %>% select(-omit)

train16 <- na.omit(train16)
test16 <- na.omit(test16)

# set seed for replicability
set.seed(123)

# train bagged model
price_bagging <- bagging(
  formula = log(num_tax_building) ~ .,
  data = train16,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

# show model
price_bagging

# compute forecasts
predict_price <- predict(price_bagging, newdata = test16)

# combine and compare with actual test data
comparison <- data.frame(seq(1,nrow(test16),1), exp(predict_price), test16$num_tax_building)
colnames(comparison) <- c( 'ind', 'prediction', 'actual')

comp <- ggplot(comparison , aes( x =  ind )) +
  geom_line(aes(y = actual), color = "black") + 
  geom_line(aes(y = prediction), color="red1", size = .25) 
comp

comp <- ggplot(comparison , aes( x =  prediction, y = actual )) +
  geom_point() +
  geom_smooth(method = lm)
comp


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


# other model
price_bagging2 <- train(
  log(num_tax_total) ~ .,
  data = train16,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 100,  
  control = rpart.control(minsplit = 2, cp = 0)
)

price_bagging2

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
    log(num_tax_total) ~ ., 
    control = rpart.control(minsplit = 2, cp = 0),
    data = train_boot
  ) 
  
  predict(bagged_tree, newdata = test16)
}


predictions %>%
  as.data.frame() %>%
  mutate(
    observation = 1:n(),
    actual = test16$num_tax_total) %>%
  tidyr::gather(tree, predicted, -c(observation, actual)) %>%
  group_by(observation) %>%
  mutate(tree = stringr::str_extract(tree, '\\d+') %>% as.numeric()) %>%
  ungroup() %>%
  arrange(observation, tree) %>%
  group_by(observation) %>%
  mutate(avg_prediction = cummean(predicted)) %>%
  group_by(tree) %>%
  summarize(RMSE = RMSE(avg_prediction, actual)) %>%
  ggplot(aes(tree, RMSE)) +
  geom_line() +
  xlab('Number of trees')


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
