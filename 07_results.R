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
errors_wide <- cbind(errors_lin,
                errors_bag, 
                errors_rf,
                errors_xgb,
                errors_stacked)

colnames(errors_wide) <- c("Linear Prediction",
                      "Bagging",
                      "Random Forest", 
                      "XGBoost", 
                      "Stacked_Generalization")

errors_long <- (data.frame(errors_wide) %>% gather(Algorithm, prediction))

# Plot boxplot
boxplot <- ggplot(errors_long, aes(x=Algorithm, y=prediction)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  ylab("Prediction Error") +
  xlab("") + 
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


# mean for errors
erroes_wide <- data.frame(errors_wide)

# t-test
t.test(errors_wide[,1])
t.test(errors_wide[,2])
t.test(errors_wide[,3])
t.test(errors_wide[,4])
t.test(errors_wide[,5])



