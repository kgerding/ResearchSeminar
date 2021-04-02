# Rsearch Seminar: Real Estate  -------------------------------------------
# Authors: Tim Graf, Kilian Gerding

# Packages used -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)



# Read in Data ------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')

# Data Inspection ---------------------------------------------------------

# Step 1: Eliminate columns with more than NAs

na_counter <- lapply(prices2016, sum(is.na(prices2016))/nrow(prices2016))



