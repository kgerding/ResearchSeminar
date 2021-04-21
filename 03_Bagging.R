# Rsearch Seminar: Bagging  ----------------------------------------------------
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

### PART 2: Bagging ------------------------------------------------------------

# define recipe
bagging_recipe <- recipes::recipe(data = train16,formula = model)

# define cross validation
cv <- vfold_cv(train16, v=5)

# Produce the model
mod_bag <- bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 10) #10 bootstrap resamples

# Create workflow
wflow_bag <- workflow() %>% 
  add_recipe(bagging_recipe) %>%
  add_model(mod_bag)

# Tuning hyperparameters
tune_spec_bag <- 
  bag_tree(tree_depth = tune()) %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 10)

#Create a regular grid of values to try using a convenience function 
bag_grid <- grid_regular(
  tree_depth(),
  levels = 5
)

#Create the workflow for the tuned bagged model 
bag_wf <- workflow() %>%
  add_formula(logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa) %>%
  add_model(tune_spec_bag)

#Tune the bagged tree model
bag_res <- tune_grid(
  wflow_bag %>% update_model(tune_spec_bag),
  cv,
  grid = bag_grid,
  metrics=metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)

show_best(bag_res, metric = "rmse")

tree_spec <- bag_tree(
  tree_depth = 8
) %>%
  set_engine("rpart", times = 100) %>%
  set_mode("regression")

bag_wf2 <- workflow() %>%
  add_formula(logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa)

tree_rs <- bag_wf2 %>%
  add_model(tree_spec) %>%
  fit(train16)

train_rs <- train16 %>%
  bind_cols(predict(tree_rs, train16))

test_rs <- test16 %>%
  bind_cols(predict(tree_rs, test16))

test_rs %>%
  metrics(logbuild, .pred)

bagging_pred <- test_rs$.pred
bagging_train <- train_rs$.pred

# metrics for train
rmse_bag_train <- sqrt(mean((bagging_train - output_vector)^2))
r2_bag_train <- 1 - ( sum((output_vector-bagging_train)^2) / sum((output_vector-mean(output_vector))^2) )
adj_r2_bag_train <- 1 - ((1 - r2_bag_train) * (nrow(output_vector) - 1)) / (nrow(output_vector) - ncol(output_vector) - 1)

# metrics for test
rmse_bag_test <- sqrt(mean((bagging_pred - test_vector)^2))
r2_bag_test <- 1 - ( sum((test_vector-bagging_pred)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_bag_test <- 1 - ((1 - r2_bag_test) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)

# combining results
results_bag_train <- rbind(rmse_bag_train, r2_bag_train, adj_r2_bag_train)
results_bag_test <- rbind(rmse_bag_test, r2_bag_test, adj_r2_bag_test)
results_bag <- data.frame(cbind(results_bag_train, results_bag_test))
colnames(results_bag) <- c("train_bag", "test_bag")
rownames(results_bag) <- c("RMSE", "R2", "ADJ_R2")

errors_bag <- bagging_pred - test_vector

# merge dataframes
merged_df <- data.frame(cbind(bagging_pred, test16)) #by 0 merges based on index
merged_df <- merged_df[order(merged_df$logbuild),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_bag <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = bagging_pred, color = 'predicted')) +
  geom_point(aes(y = logbuild, color = 'actual')) +
  ggtitle('Bagging: Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_bag

ggsave('plot_bag.png', plot = plot_bag, path = './Plots/', device = 'png')


# Plot importance
vi <- tree_rs %>%
  pull_workflow_fit()

vi <- vi$fit

vi <- vi$imp


bag_importance <- vi
bag_importance$term <- factor(bag_importance$term, levels = bag_importance$term[order(bag_importance$value)])

plot_bag_importance <- bag_importance %>%
  ggplot(aes(x=term, y=value)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  ggtitle('Feature Importance Plot for Bagging')
plot_bag_importance

ggsave('plot_bag_imp.png', plot = plot_bag_importance, path = './Plots/', device = 'png')



# assess RMSE 10-150 bagged trees (tuning)
ntree <- 10:150

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  model <- bagging(
    formula = logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa,
    data    = train16,
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err
}

# plot RSME
plot(ntree, rmse, type = 'l', lwd = 2, main = "RMSE vs. Number of N-Trees in a Bagging Model")
abline(v = 25, col = "red", lty = "dashed")