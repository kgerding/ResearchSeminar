library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

rm(list = ls())

# Read Data ---------------------------------
setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository/')

prop16 <- fread('../zillow_housing_data/properties_2016.csv')
transactions <- fread('../zillow_housing_data/train_2016_v2.csv')
sample_submission <- fread('../zillow_housing_data/sample_submission.csv')

# Exploratory Analysis -----------------------
# count missing values
missing_values <- prop16 %>% summarize_each(funs(sum(is.na(.))/n()))


# count & plot missing values
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

# good features
good_features <- filter(missing_values, missing_pct < 0.20)
features <- good_features$feature

# drop not  needed features
prop16_clean <- prop16 %>% select(features)

#count amount of 9
count_9 <- (prop16_clean$taxvaluedollarcnt==9)
count_9 <- length(count_9[count_9==TRUE])

#count amount of 10
count_10 <- (prop16_clean$taxvaluedollarcnt==10)
count_10 <- length(count_10[count_10==TRUE])

#count amount of NA
count_NA <- is.na(prop16_clean$taxvaluedollarcnt)
count_NA <- length(count_NA[count_NA==TRUE])

#drop 9 & 10s 
prop16_clean2 <- prop16_clean %>% filter(taxvaluedollarcnt != 10)
prop16_clean2 <- prop16_clean2 %>% filter(taxvaluedollarcnt != 9)
prop16_clean2 <- prop16_clean2 %>% filter_at(vars(taxvaluedollarcnt),any_vars(!is.na(.)))

prop16_clean3 <- prop16_clean[prop16_clean$taxvaluedollarcnt != 9,]
prop16_clean3 <- prop16_clean3[prop16_clean3$taxvaluedollarcnt != 10,]
prop16_clean3 <- prop16_clean3[!is.na(prop16_clean3$taxvaluedollarcnt)]

df_not_na <- prop16_clean %>% filter(!is.na(taxvaluedollarcnt))




# sanity check
nrow(prop16) - nrow(prop16_clean)
count_9 + count_10


