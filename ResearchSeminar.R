# Rsearch Seminar: Real Estate  ------------------------------------------------
# Authors: Tim Graf, Kilian Gerding

### Packages used --------------------------------------------------------------

library(tidyverse)
library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)
library(spdep)
library(dplyr)
library(maptools)
library(spatialreg)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(mapview)
library(rgeos)


rm(list=ls())

### Read in Data ---------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')
id <- read.csv('./Info on Data/id.csv', sep = ',')
heating_id <- read.csv('./Info on Data/heating_id.csv', sep = ',')
quality_id <- read.csv('./Info on Data/quality_id.csv', sep = ',')

### PART 1: DATA INSPECTION ----------------------------------------------------

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
colnames(heating_id) <- c('type_heating', 'heating_factor')
colnames(quality_id) <- c('type_quality', 'quality_factor')

## Step 1: Transform data ------------------------------------------------------

# transform dummies and factors
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa == 'true'] <- 1
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa != '1'] <- 0
p2016$flag_fireplace[p2016$flag_fireplace == 'true'] <- 1
p2016$flag_fireplace[p2016$flag_fireplace != '1'] <- 0
p2016$flag_tub_or_spa <- as.numeric(as.character(p2016$flag_tub_or_spa))
p2016$flag_fireplace <- as.numeric(as.character(p2016$flag_fireplace))

# add age insted of year_built
p2016$age <- 2021 - p2016$year_built

# type id as factor
p2016 <- left_join(p2016, id, by = 'type_zoning_landuse')
p2016 <- p2016[ , -which(names(p2016) %in% c("type_zoning_landuse"))]
p2016$factor <- as.factor(p2016$factor)

# heating id as factor
p2016 <- left_join(p2016, heating_id, by = 'type_heating')
p2016 <- p2016[ , -which(names(p2016) %in% c("type_heating"))]
p2016$heating_factor <- as.factor(p2016$heating_factor)

# type id as factor
p2016 <- left_join(p2016, quality_id, by = 'type_quality')
p2016 <- p2016[ , -which(names(p2016) %in% c("type_quality"))]
p2016$quality_factor <- as.factor(p2016$quality_factor)

# proportion of living area to area lot
p2016$prop_living <- p2016$area_live_finished/p2016$area_lot

# proportion of building to land value
p2016$build_land_prop <- p2016$num_tax_building/p2016$num_tax_land

# filter no baths and no bedrooms, we aim to separate properties with buildings
# and properties without buildings
property_only16 <- p2016[(p2016$num_bathroom == 0 & p2016$num_bedroom == 0),]
house_only16 <- p2016[(p2016$num_bathroom > 0 | p2016$num_bedroom > 0),]

## Step 4: Eliminate columns with more than 20% NAs -------------------

# first remove NAs of most important columns to see an more workable data set
# delete 7126 rows
house_only16 <- house_only16[(na.omit(house_only16$num_bathroom) &
                                na.omit(house_only16$num_bedroom) &
                                na.omit(house_only16$num_tax_total) ), ]

# quick plot
count_nas <- colSums(is.na(house_only16))/nrow(house_only16)
sorted <- rev(sort(count_nas))
barplot(sorted, cex.names = 0.5, las = 2)
abline(v=35, col="red")


# define and select hedonics (<0.2 NAs or <0.4 NAs)
hedonics <- c('id_parcel','num_bathroom','num_bedroom','area_live_finished',
              'flag_tub_or_spa','loc_latitude','loc_longitude','area_lot',
              'factor','loc_zip','loc_county', 'age',
              'flag_fireplace', 'num_tax_building','num_tax_total',
              'num_tax_land', 'prop_living', 'build_land_prop')

hedonics2 <- c('id_parcel','num_bathroom','num_bedroom','area_live_finished',
              'flag_tub_or_spa','loc_latitude','loc_longitude','area_lot',
              'factor','loc_zip','loc_county', 'age',
              'flag_fireplace', 'num_tax_building','num_tax_total',
              'num_tax_land', 'num_unit', 'quality_factor', 'heating_factor',
              'prop_living', 'build_land_prop')

house_only16_mv <- house_only16 %>% select(hedonics2)
house_only16 <- house_only16 %>% select(hedonics)

# finally remove all NAs as not usefull for regression and ML
#house_only16 <- na.omit(house_only16)
#house_only16_mv <- na.omit(house_only16_mv)


## Step 5: Eliminate properties without buildings and very low values ----------

# drop building values below 50'000
hist(house_only16$num_tax_building[house_only16$num_tax_building < 500000],
     breaks = 100)

house_only16 <- house_only16[house_only16$num_tax_total >= 50000,]

## Step 5: Plot the variable relationships and remove outliers -------------------------------
# plot bedroom vs tax
ggplot(data = house_only16[1:100000,], aes(x = num_bedroom, y = log(num_tax_building))) +
  geom_point()

# plot bedroom vs tax
ggplot(data = house_only16[1:100000,], aes(x = num_bathroom, y = log(num_tax_building))) +
  geom_point()

# plot size vs tax
ggplot(data = house_only16[1:100000,], aes(x = area_live_finished, y = (num_tax_building))) +
  geom_point()

# we need to filter the outlier of high area_live finished: 
house_only16 <- house_only16[house_only16$area_live_finished < 58000,]

ggplot(data = house_only16[1:100000,], aes(x = area_live_finished, y = (num_tax_building))) +
  geom_point() 

# plot age vs tax
ggplot(data = house_only16[1:100000,], aes(x = age, y = (log(num_tax_building)))) +
  geom_point() 

# plot area_lot vs tax
ggplot(data = house_only16[1:100000,], aes(x = area_lot, y = (num_tax_building))) +
  geom_point() 

# we need to filter the outlier of high area_lot but very low building structure value 
house_only16 <- house_only16[house_only16$area_lot < 1e7,]


### PART 2 ALGORITHMS ###-------------------------

## Regressions ------------------------------------

# simple regression of building value
hedonic_build <- lm(log(num_tax_building) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                ,data = house_only16)

summary(hedonic_build)

# simple regression of total value
hedonic_total <- lm(log(num_tax_total) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                ,
                data = house_only16)

summary(hedonic_total)


# lm of building value with factors
hedonic_total_fact <- lm(log(num_tax_building) ~ num_bathroom + num_bedroom + area_live_finished + 
                flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                + factor
                , 
                data = house_only16)

summary(hedonic_total_fact)

hedonic_total_fact2 <- lm(log(num_tax_building) ~ num_bathroom + num_bedroom + area_live_finished + 
                           flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                         + factor + num_unit + quality_factor + heating_factor
                         , 
                         data = house_only16_mv)

summary(hedonic_total_fact2)

# lm of building value with factors
hedonic_total_fact <- lm(log(num_tax_total) ~ num_bathroom + num_bedroom + area_live_finished + 
                           flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                         + factor
                         , data = house_only16)

summary(hedonic_total_fact)

hedonic_total_fact <- lm(log(num_tax_total) ~ num_bathroom + num_bedroom + area_live_finished + 
                           flag_tub_or_spa + area_lot + age + flag_fireplace #+ prop_living + build_land_prop
                         + factor + num_unit + quality_factor + heating_factor
                         , data = house_only16_mv)

summary(hedonic_total_fact_mv)

## Advanced Algorithms --------------------------------
library(xgboost)
library(Matrix)


# new data is house_only16
# or house_only16_mv for more variables

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
dtrain <- xgb.DMatrix(data = train16, label= as.matrix(train16$num_tax_total))

test16 <- p2016[-train_ind, ]
test16 <- test16 %>% select(-omit)
test16 <- as.matrix(test16)

# convert categorical factor into one-hot encoding
sparse_matrix <- sparse.model.matrix(num_tax_total~.-1, data = train16)
head(sparse_matrix)

output_vector = train16$num_tax_total

bst <- xgboost(data = train16, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgboost(data = train16, 
        booster = "gbtree", 
        objective = "binary:logistic", 
        max.depth = 5, 
        eta = 0.5, 
        nthread = 2, 
        nround = 2, 
        min_child_weight = 1, 
        subsample = 0.5, 
        colsample_bytree = 1, 
        num_parallel_tree = 1)



# Spatial Regression ------------------------------------------------------

# Step 1: Visualisation

# set longitude and latiude data right

house_only16$loc_latitude <- house_only16$loc_latitude/1000000
house_only16$loc_longitude <- house_only16$loc_longitude/1000000

data <- house_only16[1:1000,]
data <- na.omit(data)

# create a new empty leaflet map
map_CA <- leaflet()

# set the view on the map with the mean longitude and latitude, zoom in a bit
map_CA <- setView(map_CA, lng = mean(data$loc_longitude), lat = mean(data$loc_latitude), zoom = 9)

# add the tile layer on top of the map
map_CA <- addTiles(map_CA)

# add coloring according to the rent/price quantile of the real estate and it to a new column in
# the original data set
forsale <- colorQuantile("Oranges", domain =  data$num_tax_total, n = 5)

# new column
data$colors_sale <- forsale(data$num_tax_total)

# add markers with color coding
map_CA <- addCircleMarkers(map_CA, lng = data$loc_longitude, lat = data$loc_latitude,
                           radius = log(data$num_tax_total/500), stroke = F,
                           fillOpacity = 0.95, fill = T,
                           fillColor =  data$colors_sale)

# add legends
map_CA <- addLegend(map_CA, pal = forsale, values = data$num_tax_total, opacity = 0.8, title = "House Prices")

# plot
map_CA


# read CA map
CA <- readShapePoly('/Users/kiliangerding/Downloads/County_Boundary/County_Boundary.shp')
plot(CA)

# the map can be used to generate contiguity or k-nearest neighbor based weight matrices W.
# The weight matrix in turn is an important input for spatial regression analysis
# first the contiguity W. Seems to misbehave at the county borders
contiguity <- tri2nb(coordinates(CA))
plot(contiguity, coordinates(CA), col = 4, add = TRUE)
# the six-nearest neighbor matrix looks better
nearest.six <- knearneigh(coordinates(CA), k = 6, RANN = FALSE) 
nearest.six2 <- knn2nb(nearest.six) # plotting of W require the object to be of class "nb". Therefore, class conversion from "knn" to "nb"
plot(CA)
plot(nearest.six2, coordinates(CA), col = 2, add = TRUE)


m <- leaflet(data = CA)
m <- setView(m, lng = mean(data$loc_longitude), lat = mean(data$loc_latitude), zoom = 4)
m <- addProviderTiles(m, providers$OpenTopoMap)
m <- addPolygons(m, lng = data$loc_longitude, lat = data$loc_latitude, stroke = TRUE, weight = 1,
                 highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE))
m



# regression models:
# first try nonspatial OLS:
fit.ols <- lm(hp/100000 ~ dlpop, data = dat1) 
summary(fit.ols)
# lin-log model: linear in lhs of equation, log on rhs: divide coefficient by 100 
# a 1% increase in population growth is estimated to increase house prices on average by CHF 67439 


