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

setwd('/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository')

### Read in Data ---------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')
id <- read.csv('./Info on Data/id.csv', sep = ',')
heating_id <- read.csv('./Info on Data/heating_id.csv', sep = ',')
quality_id <- read.csv('./Info on Data/quality_id.csv', sep = ',')

### PART 1: DATA INSPECTION ----------------------------------------------------

rm(list=setdiff(ls(), c("prices2016", "prices2017", "id", "heating_id", "quality_id")))

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


# Step 6: save the dataframe ------------------------------------------
write.csv2(house_only16_mv, './Data/house_only16_mv')
write.csv2(house_only16, './Data/house_only16')


### PART 2 ALGORITHMS ###---------------------------------------------------

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


#########################################################
### Advanced Algorithms -----------------------------------
#########################################################

"note: Xgboost manages only numeric vectors.
For many machine learning algorithms, using correlated features is not a good idea. 
It may sometimes make prediction less accurate, and most of the time make interpretation of the model 
almost impossible. GLM, for instance, assumes that the features are uncorrelated.

Fortunately, decision tree algorithms (including boosted trees) are very robust to these features. 
Therefore we have nothing to do to manage this situation.
"

library(xgboost)
library(Matrix)
library(mlr)
library(parallel)
library(parallelMap) 
library(randomForest)



# convert to numeric, as xgboost only handles numeric values
house_only16_mv$area_live_finished <- as.numeric(house_only16_mv$area_live_finished)
house_only16_mv$num_unit <- as.numeric(house_only16_mv$num_unit)

house_only16$area_live_finished <- as.numeric(house_only16$area_live_finished)

# select the dataframe
data = na.omit(house_only16_mv)
  
  ##set the seed to make your partition reproducible
  set.seed(123)
  smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  # features we want to omit for the model 
  omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'num_tax_building', 'num_tax_land', 'factor')

  # Split the data into train and test
  train16 <- data[train_ind,]
  test16 <- data[-train_ind, ]
  
  # define training label = dependent variable
  output_vector = as.matrix(train16[,'num_tax_total'])
  test_vector = as.matrix(test16[,'num_tax_total'])
  
  #omit variables and convert to numeric again
  train16 <- train16 %>% select(-omit)
  #train16 <- data.frame(sapply(train16, as.numeric))
  test16 <- test16 %>% select(-omit)
  #test16 <- data.frame(sapply(test16, as.numeric))
  
  # convert categorical factor into dummy variables using one-hot encoding
  sparse_matrix_train <- sparse.model.matrix(num_tax_total~.-1, data = train16)
  sparse_matrix_test <- sparse.model.matrix(num_tax_total~.-1, data = test16)
  
  # check the dimnames crated by the one-hot encoding
  sparse_matrix_train@Dimnames[[2]]
  
  # they should both be of equal length
  nrow(sparse_matrix) 
  nrow(output_vector)
  
  # Create a dense matrix
  dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = output_vector)
  dtest <- xgb.DMatrix(data = sparse_matrix_test, label=test_vector)
  
  
### XGBOOST - training ###  -----------------------------

# Model 1: Default parameters  -----------------------------
#Let's start with a standard model and parameters and start optimizing the parameters later from here

params <- list(booster = "gbtree", 
                 objective = "reg:squarederror",
                 eta=0.3, # learning rate, between 0 and 1
                 gamma=0, # regularization (prevents overfitting), higher means more penality for large coef
                 max_depth=6, # max depth of trees, the more deep the more complex and overfitting
                 min_child_weight=1, # min number of instances per child node, blocks potential feature interaction and thus overfitting
                 subsample=1, # number of observations per tree, typically between 0.5 - 0.8
                 colsample_bytree=1) # number of variables per tree, typically between 0.5 - 0.9

# using cross-validation to find optimal nrounds parameter
xgbcv <- xgb.cv(params = params,
                      data = dtrain, 
                      nrounds = 100, 
                      nfold = 5,
                      showsd = T, # whether to show standard deviation of cv
                      stratified = T, 
                      print_every_n = 1, 
                      early_stopping_rounds = 20, # stop if we don't see much improvement
                      maximize = F, 
                      verbose = 2)

# Result of best iteration
xgbcv$best_iteration

# first training with optimized nround
xgb1 <- xgb.train(params = params, 
                  data = dtrain, 
                  nrounds = xgbcv$best_iteration, 
                  watchlist = list(val = dtest, train = dtrain), 
                  early_stopping_rounds = 20, 
                  maximize = F, 
                  eval_metric = "rmse"
                  )

# model prediction
xgb1_pred <- predict(xgb1, dtest)
rmse <- sqrt(mean((xgb1_pred - test_vector)^2))
print(rmse)

print(head(xgb1_pred))
print(head(test_vector))

# plot the most important leaflets
xgb.plot.multi.trees(feature_names = names(dtrain), 
                     model = xgb1)

# Plot importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb1)
xgb.plot.importance(importance_matrix = importance, top_n = 15)


# Model 2: Optimized parameters  -----------------------------

set.seed(0)

# create tasks for learner
traintask <- makeClassifTask(data = train16, target = 'num_tax_total')
testtask <- makeClassifTask(data = test16, target = 'num_tax_total')

# create dummy features, as classif.xgboost does not support factors
traintask <- createDummyFeatures(obj = traintask)
testtask <- createDummyFeatures(obj = traintask)

# create learner
# fix number of rounds and eta 
lrn <- makeLearner("classif.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror",
                     eval_metric="rmse", 
                     nrounds=100L, 
                     eta=0.1)

# set parameter space
params <- makeParamSet(makeDiscreteParam("booster",
                                         values = c("gbtree","gblinear")), 
                       makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                       makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))


# set resampling strategy
# as we don't have enough observations for certain classes we cannot do stratification
# e.g. we may not have 5 observations for a house with the factor and class 'wood'
rdesc <- makeResampleDesc("CV",stratify = F, iters=5L)

#search strategy
# instead of a grid search we use a random search strategy to find the best parameters
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)


mytune$y 




# # save model to local file
# xgb.save(model_xgb1, "xgboost.model1")
# 
# # load xgboosting model
# bst2 <- xgb.load("xgboost.model")


# model 2 (without dense matrix) - xg boosting trees -----------------------------
model_xgb2 <- xgboost(data = sparse_matrix, 
                      booster = "gbtree", 
                      label = output_vector,
                      max.depth = 4, # the depth of the trees
                      eta = 1, 
                      gamma = 0,
                      nthread = 8, # number of cpu threads
                      nrounds = 10, 
                      objective = "reg:squarederror",
                      verbose = 2) # see the training progress

# Plot importance
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = model_xgb2)
xgb.plot.importance(importance_matrix = importance)


# model 3 (measure learning progress with xgb.train) - xg boosting trees  -----------------------------
watchlist <- list(train=dtrain, test=dtest)

"about the parameters: 
  - eta: Low eta value means model is more robust to overfitting.
  - gamma: minimum loss reduction required to make a further partition on a leaf node of the tree"

params <- list(booster = "gbtree",
               objective = "reg:squarederror", 
               eta=0.3, 
               gamma=0,
               max_depth=6, 
               early_stopping_rounds = 5, 
               verbose = 2, 
) 

xgbcv <- xgb.cv( params = params, 
                 data = dtrain, 
                 nrounds = 100, 
                 nfold = 5, 
                 showsd = T, 
                 stratified = T, 
                 print_every_n = 1, 
                 early_stopping_rounds = 5, 
                 maximize = F)

model_xgb3 <- xgb.train(data=dtrain, 
                        params = params, 
                        watchlist = watchlist,
                        early_stopping_rounds = 5, 
                        nrounds = 1000)


# Plot importance
# importance <- xgb.importance(feature_names = sparse_matrix_train@Dimnames[[2]], model = model_xgb3)
importance <- xgb.importance (feature_names = colnames(sparse_matrix_train), model = model_xgb3)
xgb.plot.importance(importance_matrix = importance, top_n = 15)

xgb.plot.tree(model = model_xgb3)


#xgb.plot.deepness(model = model_xgb3, 
#                  which = c("2x1", "max.depth", "med.depth", "med.weight"))

# model 4 - linear boosting
"Note that linear boosting is great to capture linear relationships while trees are better at
capturing non-linear relationship"
model_xgb4 <- xgb.train(data=dtrain, 
                        booster = "gblinear",
                        max_depth=5,
                        nthread = 4, 
                        nrounds=10,
                        watchlist=watchlist, 
                        eval_metric = "rmse",
                        objective = "reg:squarederror",
                        verbose = 2)




# Spatial Regression ------------------------------------------------------

# Step 1: Visualisation


# new df
data <- na.omit(house_only16)

# set longitude and latiude data right
data$loc_latitude <- data$loc_latitude/1000000
data$loc_longitude <- data$loc_longitude/1000000

# subset if wanted
#data <- data[1:1000,]

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

# Step 2 : Weight Matrix

# read CA map
CA <- readShapePoly('/Users/kiliangerding/Downloads/LA_County_City_Boundaries/LA_County_City_Boundaries.shp')
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


#m <- leaflet(data = CA)
#m <- setView(m, lng = mean(data$loc_longitude), lat = mean(data$loc_latitude), zoom = 4)
#m <- addProviderTiles(m, providers$OpenTopoMap)
#m <- addPolygons(m, lng = data$loc_longitude, lat = data$loc_latitude, stroke = TRUE, weight = 1,
#                 highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE))
#m


# The SAR Model:----------------------------------------

LA <- read_sf("/Users/kiliangerding/Downloads/LA_County_City_Boundaries/LA_County_City_Boundaries.shp")

pnts <- data[, c('loc_latitude', 'loc_longitude')]
pnts_sf <- st_as_sf(pnts , coords = c('loc_latitude', 'loc_longitude'), crs = st_crs(LA))

pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, LA))
  , area = if_else(is.na(intersection), '', LA$CITY_NAME[intersection])
) 

pnts

# estimation of the spatial regression model by maximum likelihood:
coords <- coordinates(data[, c('loc_latitude', 'loc_longitude')])

k6 <- knn2nb(knearneigh(coords, k = 6))
plot(knn6, coords, pch = 19, cex = 0.6, add = TRUE, col = 'red')

#k6dists <- unlist(nbdists(k6, coords, longlat = TRUE))
#summary(k1dists)


##
W <- nb2mat(nearest.six2)

# variables are in matrix form because we need matrix algebra:
Y <- as.matrix(house_only16$num_tax_building)
colnames(Y) <- "HP"
X <- cbind(1,house_only16$age)
colnames(X) <- c("intercept","age")

# the lagsarlm() command in the spedep package requires the weight matrix to be a listw object
W.listw <- nb2listw(nearest.six2)

fit.sar <- lagsarlm(log(num_tax_building) ~ age, W.listw, data = house_only16, method="eigen", quiet = TRUE)

summary(fit.sar) # same coefficient than what we get using ML "by hand" (see fit2)

