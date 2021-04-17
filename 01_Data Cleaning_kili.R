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
ac_id <- read.csv('./Info on Data/ac_id.csv', sep = ',')


# change colnames
colnames(id) <- c('type_zoning_landuse', 'factor')
colnames(heating_id) <- c('type_heating', 'heating_factor')
colnames(quality_id) <- c('type_quality', 'quality_factor')
colnames(ac_id) <- c('type_ac', 'ac_factor')

### PART 1: DATA INSPECTION ----------------------------------------------------

# change prices to respective year 2016 or 2017
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
  
  ## Step 1: Transform data ------------------------------------------------------
  
  # transform dummies and factors
  p2016$flag_tub_or_spa[p2016$flag_tub_or_spa == 'true'] <- 1
  p2016$flag_tub_or_spa[p2016$flag_tub_or_spa != '1'] <- 0
  p2016$flag_fireplace[p2016$flag_fireplace == 'true'] <- 1
  p2016$flag_fireplace[p2016$flag_fireplace != '1'] <- 0
  p2016$flag_tub_or_spa <- as.numeric(as.character(p2016$flag_tub_or_spa))
  p2016$flag_fireplace <- as.numeric(as.character(p2016$flag_fireplace))
  
  # add age instead of year_built
  p2016$age <- 2021 - p2016$year_built
  
  # drop year_built
  p2016 <- p2016 %>% select(-'year_built')
  
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
  
  # type ac as factor
  p2016 <- left_join(p2016, ac_id, by = 'type_ac')
  p2016 <- p2016[ , -which(names(p2016) %in% c("type_ac"))]
  p2016$quality_factor <- as.factor(p2016$ac_factor)
  
  # filter no baths and no bedrooms, we aim to separate properties with buildings
  # and properties without buildings
  house_only16 <- p2016[(p2016$num_bathroom > 0 | p2016$num_bedroom > 0),]  
  house_only16 <- p2016[p2016$num_room > 0,] 
  
  ## Step 4: Eliminate columns with more than 20% NAs -------------------
  
  # NA omitting -> order is consciously chosen
  # to extract as much variation as possible from data
  # focus on Single Family Residential
  house_only16 <- house_only16[!is.na(house_only16$num_tax_building),]
  house_only16 <- house_only16[!is.na(house_only16$num_garage),]
  house_only16 <- house_only16[!is.na(house_only16$area_live_finished),]
  house_only16 <- house_only16[!is.na(house_only16$num_bathroom),]
  house_only16 <- house_only16[!is.na(house_only16$num_bedroom),]
  house_only16 <- house_only16[!is.na(house_only16$age),]
  house_only16 <- house_only16[!is.na(house_only16$factor),]
  house_only16 <- house_only16[!is.na(house_only16$area_lot),]
  house_only16 <- house_only16[!is.na(house_only16$loc_city),]
  house_only16 <- house_only16[!is.na(house_only16$num_story),]
  house_only16 <- house_only16[!is.na(house_only16$num_tax_property),]
  house_only16 <- house_only16[house_only16$factor == 'Single Family Residential',]
  
  # light data set
  house_only16_factor <- house_only16[!is.na(house_only16$quality_factor),]
  house_only16_factor <- house_only16_factor[!is.na(house_only16_factor$ac_factor),]
  
  # reintroduce 0 pools
  house_only16$num_pool[is.na(house_only16$num_pool)] <- 0
  
  # correct garage
  house_only16 <- house_only16[((house_only16$num_garage > 0 & house_only16$area_garage > 0) | (house_only16$num_garage == 0 & house_only16$area_garage == 0) ) , ]
  
  # quick plot
  count_nas <- colSums(is.na(house_only16))/nrow(house_only16)
  sorted <- rev(sort(count_nas))
  barplot(sorted, cex.names = 0.5, las = 2)
  abline(v=30, col="red")
  title(main = '% NAs in 2016 Data')
  
  # omit variables with high NAs or no conceptual use
  
  omit <- c('id_parcel','area_basement', 'loc_county', 'area_liveperi_finished',
            'type_framing', 'area_total_finished', 'num_unit', 'type_story',
            'type_architect', 'type_material', 'tax_delinquency_flag',
            'tax_delinquency_year', 'flag_spa_or_tub_pool', 'flag_spa_or_tub',
            'heating_factor', 'type_deck', 'ac_factor', 'area_pool',
            'quality_factor', 'area_patio', 'flag_no_tub' , 'num_75_bath',
            'num_fireplace', 'area_firstfloor_finished', 'area_shed',
            'num_bathroom_calc', 'loc_neighbor','area_unknown', 'area_base',
            'loc_fips', 'loc_tract_block', 'loc_raw_tract_block',
            'type_desc_zoning_landuse', 'type_zoning_landuse_county', 'factor',
            'num_tax_land', 'tax_assess_year', 'num_bath', 'area_total_calc',
            'num_room'
            )

  house_only16 <- house_only16 %>% select(-omit)
  
  # histrogram plotting
  columns <- c('num_tax_building', 'area_live_finished', 'area_lot',
               'num_story','num_bedroom', 'num_bathroom',
               'num_garage', 'num_pool', 'age')
  
  subhouse_only16 <- house_only16[,columns]
  hist.data.frame(subhouse_only16)
  
  # compile long and lat data in H3
  library(h3)
  
  # define coordinates
  coords <- cbind(house_only16$loc_latitude/1000000,house_only16$loc_longitude/1000000)
  
  #define resolution of H3 indexes
  resolution <- 6
  
  # Convert a lat/lng point to a hexagon index at resolution 7
  h3_index <- geo_to_h3(coords, resolution)
  
  # rendering hexagons
  tbl <- table(h3_index) %>%
    tibble::as_tibble()
  hexagons <- h3_to_geo_boundary_sf(tbl$h3_index) %>%
    dplyr::mutate(index = tbl$h3_index, accidents = tbl$n)
  
  library(leaflet)
  
  pal <- colorBin("YlOrRd", domain = hexagons$accidents)
  
  map <- leaflet(data = hexagons, width = "100%") %>%
    addProviderTiles("Stamen.Toner") %>%
    addPolygons(
      weight = 2,
      color = "white",
      fillColor = ~ pal(accidents),
      fillOpacity = 0.8,
      label = ~ sprintf("%i accidents (%s)", accidents, index)
    )
  
  map
  
  # Get the center of the hexagon
  h3_to_geo_sf(h3_index)
  
  
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
    ggtitle("2016 House Price vs # bedrooms, Random subset of 100k") +
    theme_bw()
  ggsave("2016bed.jpeg", height = 7, width = 6, dpi=700)
  
  # plot bedroom vs tax
  ggplot(data =  sample, aes(x = num_bathroom, y = log(num_tax_building))) +
    geom_point() + 
    ggtitle("2016 House Price vs # bathrooms, Random subset of 100k") +
    theme_bw()
  ggsave("2016bath.jpeg", height = 7, width = 6, dpi=700)
  
  # plot size vs tax
  ggplot(data = sample, aes(x = area_live_finished, y = (num_tax_building))) +
    geom_point() +
    ggtitle("2016 House Price vs living area, Random subset of 100k") +
    theme_bw()
  ggsave("2016area.jpeg", height = 7, width = 6, dpi=700)

  # plot age vs tax
  ggplot(data = sample, aes(x = age, y = (log(num_tax_building)))) +
    geom_point() +
    ggtitle("2016 House Price vs Age, Random subset of 100k") +
    theme_bw()
  ggsave("2016age.jpeg", height = 7, width = 6, dpi=700)
  
  # plot area_lot vs tax
  ggplot(data = sample, aes(x = area_lot, y = (num_tax_building))) +
    geom_point() +
    ggtitle("2016 House Price vs lot size, Random subset of 100k") +
    theme_bw()
  ggsave("2016lot.jpeg", height = 7, width = 6, dpi=700)
  
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
  
  write.csv(house_only16_mv, file = 'house_only16_mv.csv')
  
  # simple regression of building value
  hedonic_build <- lm(logbuild ~ num_bathroom + num_bedroom + log(area_live_finished) + 
                  flag_tub_or_spa + log(age) + flag_fireplace #+ prop_living + build_land_prop
                  ,data = house_only16)
  
  summary(hedonic_build)
  #plot(hedonic_build$residuals)
  bptest(hedonic_build)
  hedonic_build_robust <- coeftest(hedonic_build, vcov = vcovHC(hedonic_build, type = "HC0"))
  
  # simple regression of total value
  hedonic_total <- lm(logtotal ~ num_bathroom + num_bedroom +  
                  flag_tub_or_spa + log(area_lot) + log(age) + flag_fireplace #+ prop_living + build_land_prop
                  ,
                  data = house_only16)
  
  summary(hedonic_total)
  #plot(hedonic_total$residuals)
  bptest(hedonic_total)
  hedonic_total_robust <- coeftest(hedonic_total, vcov = vcovHC(hedonic_total, type = "HC0"))
  
  # lm of building value with factors
  
  hedonic_build_fact <- lm(logbuild ~ num_bathroom + num_bedroom + log(area_live_finished) + 
                             flag_tub_or_spa + log(age) + flag_fireplace #+ prop_living + build_land_prop
                           + factor + num_unit + quality_factor + heating_factor
                           , 
                           data = house_only16_mv)
  
  summary(hedonic_build_fact)
  bptest(hedonic_build_fact)
  hedonic_build_fact_robust <- coeftest(hedonic_build_fact, vcov = vcovHC(hedonic_build_fact, type = "HC0"))
  
  # lm of building value with factors
  hedonic_total_fact <- lm(logtotal ~ num_bathroom + num_bedroom +  
                             flag_tub_or_spa + log(area_lot) + log(age) + flag_fireplace #+ prop_living + build_land_prop
                           + factor + num_unit + quality_factor + heating_factor
                           , data = house_only16_mv) 
  
  summary(hedonic_total_fact)
  hedonic_total_fact_robust <- coeftest(hedonic_total_fact, vcov = vcovHC(hedonic_total_fact, type = "HC0"))
  bptest(hedonic_total_fact)
  
  # build nice regression table
  
  library(jtools)
  library(huxtable)
  library(officer)
  library(flextable)
  
  export_summs(hedonic_build_robust,
               hedonic_build_fact_robust,
               number_format = "%.3f",
               file.name = "2017building.docx", to.file = 'docx')
  
  export_summs(hedonic_total_robust,
               hedonic_total_fact_robust,
               number_format = "%.3f",
               file.name = "2017total.docx", to.file = 'docx',
               scale = FALSE, robust = TRUE)
}

## Advanced Algorithms --------------------------------

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

