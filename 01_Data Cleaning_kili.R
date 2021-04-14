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
library(lmtest)
library(sandwich)


rm(list=ls())

### Read in Data ---------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')

# additional factor description
id <- read.csv('./Info on Data/id.csv', sep = ',')
heating_id <- read.csv('./Info on Data/heating_id.csv', sep = ',')
quality_id <- read.csv('./Info on Data/quality_id.csv', sep = ',')

# change colnames
colnames(id) <- c('type_zoning_landuse', 'factor')
colnames(heating_id) <- c('type_heating', 'heating_factor')
colnames(quality_id) <- c('type_quality', 'quality_factor')

### PART 1: DATA INSPECTION ----------------------------------------------------

#all_data <- list(prices2016, prices2017)

for (i in c(1,2)) {

  ## Step 0: Rename the Data
  
  i <- all_data[[1]]
  
  p2016 <- i %>% rename(
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
  title(main = '% NAs in 2017 Data')
  ggsave("2017NAs.jpeg", height = 7, width = 6, dpi=700)
  
  
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
  
  set.seed(123)
  
  sample <- sample_n(house_only16, 100000)
  
  
  # plot bedroom vs tax
  ggplot(data = sample, aes(x = num_bedroom, y = log(num_tax_building))) +
    geom_point() + 
    ggtitle("2017 House Price vs # bedrooms, Random subset of 100k") +
    theme_bw()
  ggsave("2017bed.jpeg", height = 7, width = 6, dpi=700)
  
  # plot bedroom vs tax
  ggplot(data =  sample, aes(x = num_bathroom, y = log(num_tax_building))) +
    geom_point() + 
    ggtitle("2017 House Price vs # bathrooms, Random subset of 100k") +
    theme_bw()
  ggsave("2017bath.jpeg", height = 7, width = 6, dpi=700)
  
  # plot size vs tax
  ggplot(data = sample, aes(x = area_live_finished, y = (num_tax_building))) +
    geom_point() +
    ggtitle("2017 House Price vs living area, Random subset of 100k") +
    theme_bw()
  ggsave("2017area.jpeg", height = 7, width = 6, dpi=700)

  # plot age vs tax
  ggplot(data = sample, aes(x = age, y = (log(num_tax_building)))) +
    geom_point() +
    ggtitle("2017 House Price vs Age, Random subset of 100k") +
    theme_bw()
  ggsave("2017age.jpeg", height = 7, width = 6, dpi=700)
  
  # plot area_lot vs tax
  ggplot(data = sample, aes(x = area_lot, y = (num_tax_building))) +
    geom_point() +
    ggtitle("2017 House Price vs lot size, Random subset of 100k") +
    theme_bw()
  ggsave("2017lot.jpeg", height = 7, width = 6, dpi=700)
  
  # some filtering of outliers
  
  # we need to filter the outlier of high area_live finished: 
  house_only16 <- house_only16[house_only16$area_live_finished < 58000,]
  house_only16_mv <- house_only16_mv[house_only16_mv$area_live_finished < 58000,]
  
  # we need to filter the outlier of high area_lot but very low building structure value 
  house_only16 <- house_only16[house_only16$area_lot < 2e6,]
  house_only16_mv <- house_only16_mv[house_only16_mv$area_lot < 2e6,]
  
  ### PART 2 ALGORITHMS ###---------------------------------------------------
  
  ## Regressions ------------------------------------
  
  house_only16$logbuild <- log(house_only16$num_tax_building)
  house_only16$logtotal <- log(house_only16$num_tax_total)
  house_only16_mv$logbuild <- log(house_only16_mv$num_tax_building)
  house_only16_mv$logtotal <- log(house_only16_mv$num_tax_total)
  
  # simple regression of building value
  hedonic_build <- lm(logbuild ~ num_bathroom + num_bedroom + log(area_live_finished) + 
                  flag_tub_or_spa + age + flag_fireplace #+ prop_living + build_land_prop
                  ,data = house_only16)
  
  summary(hedonic_build)
  #plot(hedonic_build$residuals)
  bptest(hedonic_build)
  hedonic_build_robust <- coeftest(hedonic_build, vcov = vcovHC(hedonic_build, type = "HC0"))
  
  # simple regression of total value
  hedonic_total <- lm(logtotal ~ num_bathroom + num_bedroom +  
                  flag_tub_or_spa + log(area_lot) + age + flag_fireplace #+ prop_living + build_land_prop
                  ,
                  data = house_only16)
  
  summary(hedonic_total)
  #plot(hedonic_total$residuals)
  bptest(hedonic_total)
  hedonic_total_robust <- coeftest(hedonic_total, vcov = vcovHC(hedonic_total, type = "HC0"))
  
  # lm of building value with factors
  
  hedonic_build_fact <- lm(logbuild ~ num_bathroom + num_bedroom + log(area_live_finished) + 
                             flag_tub_or_spa + age + flag_fireplace #+ prop_living + build_land_prop
                           + factor + num_unit + quality_factor + heating_factor
                           , 
                           data = house_only16_mv)
  
  summary(hedonic_build_fact)
  bptest(hedonic_build_fact)
  hedonic_build_fact_robust <- coeftest(hedonic_build_fact, vcov = vcovHC(hedonic_build_fact, type = "HC0"))
  
  # lm of building value with factors
  hedonic_total_fact <- lm(logtotal ~ num_bathroom + num_bedroom +  
                             flag_tub_or_spa + log(area_lot) + age + flag_fireplace #+ prop_living + build_land_prop
                           + factor + num_unit + quality_factor + heating_factor
                           , data = house_only16_mv) 
  
  summary(hedonic_total_fact)
  hedonic_total_fact_robust <- coeftest(hedonic_total_fact, vcov = vcovHC(hedonic_total_fact, type = "HC0"))
  
  # build nice regression table
  
  library(jtools)
  library(huxtable)
  library(officer)
  library(flextable)
  
  export_summs(hedonic_build_robust,
               hedonic_build_fact_robust,
               number_format = "%.3f",
               file.name = "2016building.docx", to.file = 'docx')
  
  export_summs(hedonic_total_robust,
               hedonic_total_fact_robust,
               number_format = "%.3f",
               file.name = "2016total.docx", to.file = 'docx',
               scale = FALSE, robust = TRUE)
}

## Advanced Algorithms --------------------------------
library(xgboost)
library(Matrix)

prep_xgboost <- function (data){
  ## set the seed to make your partition reproducible
  set.seed(123)
  smp_size <- floor(0.75 * nrow(data)) ## 75% of the sample size
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  # featues we want to omit for the model 
  omit <- c('id_parcel', 'loc_latitude', 'loc_longitude', 'loc_zip', 'loc_county', 'num_tax_building', 'num_tax_land', 'factor')
  # note: Xgboost manages only numeric vectors.
  
  # Split the data into train and test
  train16 <- data[train_ind,]
  train16 <- train16 %>% select(-omit)
  train16 <- as.matrix(train16)
  dtrain <- xgb.DMatrix(data = train16, label= as.matrix(train16$num_tax_total))
  
  test16 <- data[-train_ind, ]
  test16 <- test16 %>% select(-omit)
  test16 <- as.matrix(test16)
  
  # convert categorical factor into one-hot encoding
  sparse_matrix <- sparse.model.matrix(num_tax_total~.-1, data = data)

  # define training label = dependent variable
  output_vector = as.matrix(trial1$num_tax_total)
}



bst <- xgboost(data = sparse_matrix, booster = "gbtree", label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "reg:linear")

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


# Bagging Predictor ------------------------------------------------------------



# Spatial Regression -----------------------------------------------------------

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

