#########################################################
### Stacked Generalization -----------------------------------
#########################################################

### SETUP ###------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)

setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')
rm(list=ls())

### MERGE ###------------------------------------------------------

# load the errors 
load("./Models/errors_stacked.RData")
load("./Models/errors_xgb.RData")
load("./Models/errors_randomforest.RData")
load("./Models/errors_bagging.RData")
load("./Models/errors_lm.RData")

#errors <- cbind(errors_stacked, errors_xgb, errors_randomforest, errors_bagging, errors_lm)
errors <- cbind(errors_stacked, errors_xgb)
colnames(errors) <- c("Stacked_Generalization", "XGBoost")
errors <- (data.frame(errors) %>% gather(Algorithm, prediction))

# Plot boxplot
boxplot <- ggplot(errors, aes(x=Algorithm, y=prediction)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  ggtitle('Predicted Values of all Algorithms')
boxplot

# save boxplot
ggsave('boxplot.png', path = './Plots/', plot = boxplot, device = 'png')



# load the aggregated RMSE, R2, Adj. R2
load("./Models/results_stacked.RData")
load("./Models/results_xgboost.RData")
load("./Models/results_randomforest.RData")
load("./Models/results_bagging.RData")
load("./Models/results_lm.RData")

#results <- cbind(results_stacked, results_xgb, results_randomforest, results_bagging, results_lm)
all_results_train <- cbind(results_stacked['train_stacked'], results_xgb['train_xgb'])
all_results_test <- cbind(results_stacked['test_stacked'], results_xgb['test_xgb'])

# save both
save(all_results_train, file = "./Models/all_results_train")
save(all_results_test, file = "./Models/all_results_test")


# t-test
t.test(errors)



