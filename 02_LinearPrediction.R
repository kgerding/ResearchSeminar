#########################################################
### LINEAR REGRESSION-----------------------------------
# Authors: Tim Graf, Kilian Gerding
#########################################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(foreach)
library(data.table)


### PART 1: Prep Data ----------------------------------------------------------

# Reading data
data <- fread('/Users/kiliangerding/Documents/GitHub/ResearchSeminar/Data/2016.csv', drop = 'V1')

# define logs for simplicity
data$logbuild <- log(data$num_tax_building)
data$logarea <- log(data$area_live_finished)
data$logage <- log(data$age)
data$loglot <- log(data$area_lot)
data$loggarage <- ifelse(data$num_garage > 0,log(data$num_garage),0)

# Regressions 
model <- logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa

# omit variables
omit <- c( 'loc_latitude', 'loc_longitude', 'num_tax_property', 'num_tax_total')

# omit
data <- data%>% select(-omit)

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

### PART 2: Prediction of linear Model ----------------------------------------

# linear model specification
hedonic_build <- lm(model, data = train16)

# prediction with linear models
hedonic_pred_train = predict(hedonic_build, newdata = train16)
hedonic_pred = predict(hedonic_build, newdata = test16)

# plotting

plot(hedonic_pred, test16$logbuild,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "blue", pch = 20)
grid()
abline(0, 1, col = "red", lwd = 2)

# metrics for train
rmse_lin_train <- sqrt(mean((hedonic_pred_train - output_vector)^2))
r2_lin_train <- 1 - ( sum((output_vector-hedonic_pred_train)^2) / sum((output_vector-mean(output_vector))^2) )
adj_r2_lin_train <- 1 - ((1 - r2_lin_train) * (nrow(output_vector) - 1)) / (nrow(output_vector) - ncol(output_vector) - 1)

# metrics for test
rmse_lin_test <- sqrt(mean((hedonic_pred - test_vector)^2))
r2_lin_test <- 1 - ( sum((test_vector-hedonic_pred)^2) / sum((test_vector-mean(test_vector))^2) )
adj_r2_lin_test <- 1 - ((1 - r2_lin_test) * (nrow(test_vector) - 1)) / (nrow(test_vector) - ncol(test_vector) - 1)

# combining results
results_lin_train <- rbind(rmse_lin_train, r2_lin_train, adj_r2_lin_train)
results_lin_test <- rbind(rmse_lin_test, r2_lin_test, adj_r2_lin_test)
results_lin <- data.frame(cbind(results_lin_train, results_lin_test))
colnames(results_lin) <- c("train_lin", "test_lin")
rownames(results_lin) <- c("RMSE", "R2", "ADJ_R2")

errors_lin <- hedonic_pred - test_vector

# merge dataframes
merged_df <- data.frame(cbind(hedonic_pred, test16)) #by 0 merges based on index
merged_df <- merged_df[order(merged_df$logbuild),]
merged_df$initialindex <- row.names(merged_df)
row.names(merged_df) <- NULL

# Plot predicted vs. actual 
colors <- c("actual" = "red", "predicted" = "blue")
plot_lin <- ggplot(data = merged_df, aes(x = as.numeric(row.names(merged_df)))) +
  geom_point(aes(y = hedonic_pred, color = 'predicted')) +
  geom_point(aes(y = logbuild, color = 'actual')) +
  ggtitle('Hedonic Regression: Actual vs. predicted values') + 
  scale_color_manual(values = colors) +
  labs(x = 'Index', y = 'Log(num_tax_building)')
plot_lin

ggsave('plot_ln_v1.png', plot = plot_lin, path = './Plots/', device = 'png')


# save results
save(results_lin,file="./Models/results_lin.RData")

# save errors
save(errors_lin, file = "./Models/errors_lin.RData")
