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

properties <- fread('../zillow_housing_data/properties_2016.csv')
transactions <- fread('../zillow_housing_data/train_2016_v2.csv')
sample_submission <- fread('../zillow_housing_data/sample_submission.csv')

#install.packages('corrplot')
#install.packages('leaflet')
#install.packages('lubridate')

missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_features <- filter(missing_values, missing_pct<0.75)