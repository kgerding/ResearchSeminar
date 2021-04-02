# Rsearch Seminar: Real Estate  -------------------------------------------
# Authors: Tim Graf, Kilian Gerding

### Packages used -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)
library(spdep)
library(dplyr)


rm(list=ls())

### Read in Data ------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')
id <- read.csv('./Info on Data/id.csv', sep = ',')

### PART 1: DATA INSPECTION ---------------------------------------------------------

## Step 0: Rename the Data

p2016 <- prices2016 %>% rename(
  id_parcel = parcelid,
  year_built = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  num_tax_total = taxvaluedollarcnt,
  num_tax_building = structuretaxvaluedollarcnt,
  num_tax_land = landtaxvaluedollarcnt,
  num_tax_property = taxamount,
  tax_assess_year = assessmentyear,
  tax_delinquency_flag = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  loc_county = regionidcounty,
  loc_city = regionidcity,
  loc_zip = regionidzip,
  loc_neighbor = regionidneighborhood, 
  loc_fips = fips,
  loc_tract_block = censustractandblock,
  loc_raw_tract_block = rawcensustractandblock,
  loc_longitude = longitude,
  loc_latitude = latitude,
  flag_fireplace = fireplaceflag, 
  flag_tub_or_spa = hashottuborspa,
  flag_spa_or_tub_pool = pooltypeid2,
  flag_no_tub = pooltypeid7,
  flag_spa_or_tub = pooltypeid10,
  type_desc_zoning_landuse = propertyzoningdesc,
  type_zoning_landuse = propertylandusetypeid,
  type_zoning_landuse_county = propertycountylandusecode,
  type_quality = buildingqualitytypeid,
  type_framing = buildingclasstypeid,
  type_material = typeconstructiontypeid,
  type_deck = decktypeid,
  type_story = storytypeid,
  type_heating = heatingorsystemtypeid,
  type_ac = airconditioningtypeid,
  type_architect = architecturalstyletypeid
)

colnames(id) <- c('type_zoning_landuse', 'factor')

## Step 1: Eliminate columns with more than 20% NAs -------------------

# quick plot
count_nas <- colSums(is.na(p2016))/nrow(p2016)
sorted <- rev(sort(count_nas))
barplot(sorted, cex.names = 0.5, las = 2)
abline(v=35, col="red")

#delete columns
p2016 <- p2016[, colSums(is.na(p2016)/nrow(p2016)) < 0.2]


## Step 2: Eliminate columns manually which are representing very similar values -------------------

# select hedonics
hedonics <- c('id_parcel','num_bathroom','num_bedroom','area_live_finished',
              'flag_tub_or_spa','loc_latitude','loc_longitude','area_lot',
              'type_zoning_landuse','loc_zip','loc_county', 'year_built', 'flag_fireplace', 'num_tax_building',
              'num_tax_total', 'num_tax_land')
p2016 <- p2016 %>% select(hedonics)

## Step 3: Transform data -------------------

# transform dummies and factors
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa == 'true'] <- 1
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa != '1'] <- 0
p2016$flag_fireplace[p2016$flag_fireplace == 'true'] <- 1
p2016$flag_fireplace[p2016$flag_fireplace != '1'] <- 0
p2016$flag_tub_or_spa <- as.numeric(as.character(p2016$flag_tub_or_spa))
p2016$flag_fireplace <- as.numeric(as.character(p2016$flag_fireplace))
p2016$age <- 2021 - p2016$year_built

# type id as factor
p2016 <- left_join(p2016, id, by = 'type_zoning_landuse')
p2016 <- p2016[ , -which(names(p2016) %in% c("type_zoning_landuse"))]
p2016$factor <- as.factor(p2016$factor)

# filter no baths and no bedrooms, we aim to separate properties with buildings and properties without buildings
nobathsorbeds <- filter(p2016, num_bathroom == 0 & num_bedroom == 0)
nrow(nobathsorbeds)/nrow(p2016)

## Step 3.1: Adding new features -------------------
# proportion of living area to area lot
p2016$prop_living <- p2016$area_live_finished/p2016$area_lot

# proportion of building to land value
p2016$build_land_prop <- p2016$num_tax_building/p2016$num_tax_land


## Step 4: Elimante properties without buildings and very low values -------------------

# drop no baths and no bedrooms
p2016 <- filter(p2016, num_bathroom != 0 & num_bedroom != 0)

# drop building values below 50'000
hist(p2016$num_tax_building[p2016$num_tax_building < 500000], breaks = 100)
p2016 <- p2016[p2016$num_tax_total >= 50000,]
p2016 <- p2016[p2016$num_bedroom >= 0,]
p2016 <- p2016[p2016$num_bathroom >= 0,]


## Step 5: Plot the variable relationships and remove outliers -------------------------------
# plot bedroom vs tax
ggplot(data = p2016[1:100000,], aes(x = num_bedroom, y = log(num_tax_building))) +
  geom_point()

# plot bedroom vs tax
ggplot(data = p2016[1:100000,], aes(x = num_bathroom, y = log(num_tax_building))) +
  geom_point()

# plot size vs tax
ggplot(data = p2016[1:100000,], aes(x = area_live_finished, y = (num_tax_building))) +
  geom_point() 
# we need to filter the outlier of high area_live finished: 
p2016 <- filter(p2016, area_live_finished < 58000)

ggplot(data = p2016[1:100000,], aes(x = area_live_finished, y = (num_tax_building))) +
  geom_point() 

# plot age vs tax
ggplot(data = p2016[1:100000,], aes(x = age, y = (log(num_tax_building)))) +
  geom_point() 

# plot area_lot vs tax
ggplot(data = p2016[1:100000,], aes(x = area_lot, y = (num_tax_building))) +
  geom_point() 
# we need to filter the outlier of high area_lot but very low building structure value 
p2016 <- filter(p2016, area_lot < 1e7)


## Step 6: Remove NAs -------------------------------
p2016 <- na.omit(p2016)

### PART 2 ALGORITHMS ###-------------------------

## Regressions ------------------------------------

# simple regression of building value
hedonic_build <- lm(log(num_tax_building) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + age + flag_fireplace + prop_living + build_land_prop, data = p2016)
summary(hedonic_build)

# simple regression of total value
hedonic_total <- lm(log(num_tax_total) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + age + flag_fireplace + prop_living + build_land_prop, data = p2016)
summary(hedonic_total)

# lm of building value with factors
hedonic_total_fact <- lm(log(num_tax_building) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + year_built + flag_fireplace + prop_living + build_land_prop + factor, data = p2016)
summary(hedonic_total_fact)

# lm of building value with factors
hedonic_total_fact <- lm(log(num_tax_total) ~ num_bathroom + num_bedroom + area_live_finished + 
                           flag_tub_or_spa + area_lot + year_built + flag_fireplace + prop_living + build_land_prop + factor, data = p2016)
summary(hedonic_total_fact)

## Advanced Algorithms --------------------------------
library(xgboost)
library(Matrix)

# separate training and testing data
smp_size <- floor(0.75 * nrow(p2016)) ## 75% of the sample size

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(p2016)), size = smp_size)

# featues we want to omit for the model 
omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'year_built', 'num_tax_building', 'num_tax_land', 'factor')
# note: Xgboost manages only numeric vectors.


# split the data
train16 <- p2016[train_ind,]
train16 <- train16 %>% select(-omit)
train16 <- as.matrix(train16)
dtrain <- xgb.DMatrix(data = train16, label= train_labels)

test16 <- p2016[-train_ind, ]
test16 <- test16 %>% select(-omit)
test16 <- as.matrix(test16)

# convert categorical factor into one-hot encoding
sparse_matrix <- sparse.model.matrix(num_tax_total~.-1, data = train16)
head(sparse_matrix)

output_vector = train16$num_tax_total

bst <- xgboost(data = train16, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")





