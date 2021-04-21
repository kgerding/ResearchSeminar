#########################################################
### DATA CLEANING-----------------------------------
# Authors: Tim Graf, Kilian Gerding
#########################################################

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
  library(Hmisc)
  library(psych)
  library(olsrr)
  library(jtools)
  library(huxtable)
  library(officer)
  library(flextable)
  
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
  
  #################################################
  # change prices to respective year 2016 or 2017 #
  #################################################
  
  priceyear <- prices2016
  
  p2016 <- priceyear %>% rename(
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
  house_only16 <- p2016[(p2016$num_bedroom > 0),] 
  house_only16 <- house_only16[(house_only16$num_bathroom > 0),] 
  
## Step 2: Eliminate NAs -------------------------------------------------------
  
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
  house_only16 <- house_only16[!is.na(house_only16$num_story),]
  house_only16 <- house_only16[!is.na(house_only16$num_tax_property),]
  
  # since we focus only on single family residential
  # 0 bed or 0 baths does not make sense to include
  house_only16 <- house_only16[house_only16$factor == 'Single Family Residential',]
  
  # reintroduce 0 pools
  house_only16$num_pool[is.na(house_only16$num_pool)] <- 0
  
  # correct garage
  house_only16 <- house_only16[((house_only16$num_garage > 0 & house_only16$area_garage > 0) | (house_only16$num_garage == 0 & house_only16$area_garage == 0) ) , ]
  
  # quick plot
  count_nas <- colSums(is.na(house_only16))/nrow(house_only16)
  sorted <- rev(sort(count_nas))
  barplot(sorted, cex.names = 0.5, las = 2)
  abline(v=30, col="red")
  title(main = '% NAs in 2017 Data')
  
  # omit variables with high NAs or no conceptual use
  omit <- c('id_parcel','area_basement', 'loc_county', 'loc_city', 'loc_zip',
            'area_liveperi_finished', 'type_framing', 'area_total_finished',
            'num_unit', 'type_story', 'type_architect', 'type_material',
            'tax_delinquency_flag', 'tax_delinquency_year',
            'flag_spa_or_tub_pool', 'flag_spa_or_tub',
            'heating_factor', 'type_deck', 'ac_factor', 'area_pool',
            'quality_factor', 'area_patio', 'flag_no_tub' , 'num_75_bath',
            'num_fireplace', 'area_firstfloor_finished', 'area_shed',
            'num_bathroom_calc', 'loc_neighbor','area_unknown', 'area_base',
            'loc_fips', 'loc_tract_block', 'loc_raw_tract_block',
            'type_desc_zoning_landuse', 'type_zoning_landuse_county', 'factor',
            'num_tax_land', 'tax_assess_year', 'num_bath', 'area_total_calc',
            'num_room'
            )
  # omit variables
  house_only16 <- house_only16 %>% select(-omit)
  
  # histrogram plotting
  columns <- c('num_tax_building', 'area_live_finished', 'area_lot',
               'num_story','num_bedroom', 'num_bathroom',
               'num_garage', 'num_pool', 'age')
  
  # big histogram plot
  subhouse_only16 <- house_only16[,columns]
  hist.data.frame(subhouse_only16)
  
  # some filtering of outliers - detection done by plotting (see below)
  
  # we need to filter the outlier of high area_live finished: 
  house_only16 <- house_only16[house_only16$num_tax_building < 7500000,]
  house_only16 <- house_only16[house_only16$age < 150,]
  
  # we need to filter the outlier of high area_live finished: 
  house_only16 <- house_only16[house_only16$area_live_finished < 20000,]
  
  # we need to filter the outlier of high area_lot
  # but very low building structure value 
  house_only16 <- house_only16[house_only16$area_lot < 100000,]
  
## Step 3: Plot the variable relationships and remove outliers -----------------
  
  # plot bedroom vs tax
  ggplot(data = house_only16, aes(x = num_bedroom, y = log(num_tax_building))) +
    geom_point() + 
    ggtitle("2017 House Price vs # bedrooms") +
    theme_bw()
  
  # plot bathroom vs tax
  ggplot(data =  house_only16, aes(x = num_bathroom, y = log(num_tax_building))) +
    geom_point() + 
    ggtitle("2017 House Price vs # bathrooms") +
    theme_bw()
  
  # plot size vs tax
  ggplot(data = house_only16, aes(x = area_live_finished, y = (num_tax_building))) +
    geom_point() +
    ggtitle("2017 House Price vs living area") +
    theme_bw()

  # plot age vs tax
  ggplot(data = house_only16, aes(x = age, y = (log(num_tax_building)))) +
    geom_point() +
    ggtitle("2017 House Price vs Age") +
    theme_bw()
  
  # plot area_lot vs tax
  ggplot(data = house_only16, aes(x = area_lot, y = num_tax_building)) +
    geom_point() +
    ggtitle("2017 House Price vs lot size") +
    theme_bw()
  
### PART 2 ALGORITHMS ###-------------------------------------------------------
  
  # define logs for simplicity
  house_only16$logbuild <- log(house_only16$num_tax_building)
  house_only16$logarea <- log(house_only16$area_live_finished)
  house_only16$logage <- log(house_only16$age)
  house_only16$loglot <- log(house_only16$area_lot)
  house_only16$loggarage <- ifelse(house_only16$num_garage > 0,log(house_only16$num_garage),0)
  
  # Regressions 
  model <- logbuild ~ logarea + loglot + loggarage + logage + num_bedroom + num_bathroom + num_story + num_garage + num_pool + flag_fireplace + flag_tub_or_spa
  
  # simple regression of building value
  hedonic_build <- lm(model,data = house_only16)
  
  # output
  summary(hedonic_build)
  
  # plot residuals
  plot(hedonic_build$residuals)
  
  # Breusch Pagan test for heteroscedasticity
  bptest(hedonic_build)
  
  # robsut standard erros
  hedonic_build_robust <- coeftest(hedonic_build, vcov = vcovHC(hedonic_build, type = "HC0"))
  
  #plot residuals
  qqnorm(hedonic_build$residuals, pch = 1, frame = FALSE)
  qqline(hedonic_build$residuals, col = "steelblue", lwd = 2)

  # check multicollinearity
  ols_vif_tol(hedonic_build)
  
  # Relative importance of independent variables in determining Y. How much
  # each variable uniquely contributes to R2 over and above that which can be
  # accounted for by the other predictors.
  ols_correlations(hedonic_build)
  
  # build nice regression table
  export_summs(hedonic_build_robust,
               number_format = "%.3f",
               file.name = "2017building.docx", to.file = 'docx')

# Spatial Regression -----------------------------------------------------------  
  
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
  
  # Get the center of the hexagon
  centers <- data.frame(h3_to_geo(h3_index))
  
  # create index 
  centers <- data.table(centers)
  centers[,Zone:= .GRP, by = c('lat', 'lng')]
  
  # cbind centers to objects
  step <- cbind(house_only16, centers)
  
  # aggregate to Zones
  agg2016 <- aggregate(step, list(step$Zone), mean)
  
  # plot map
  map <- leaflet(data = hexagons, height=4000, width=4000) %>%
    addProviderTiles("Stamen.Toner") %>%
    addPolygons(
      weight = 2,
      color = "white",
      fillColor = ~ pal(accidents),
      fillOpacity = 0.8,
      label = ~ sprintf("%i accidents (%s)", accidents, index)
    ) %>%
    addLabelOnlyMarkers(data = agg2016,
                        lng = agg2016$lng, lat = agg2016$lat, label = agg2016$Zone,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE))
  
  map
  
  # detect spatial autocorrelation
  la.dists <- as.matrix(dist(cbind(agg2016$lng, agg2016$lat)))
  la.dists.inv <- 1/la.dists
  diag(la.dists.inv) <- 0
  
  library(ape)
  Moran.I(agg2016$num_tax_building, la.dists.inv)
  
  # basic OLS per zone 
  model <- log(num_tax_building) ~ num_bathroom + num_bedroom + log(area_live_finished) + flag_tub_or_spa + log(age) + flag_fireplace + num_garage + num_pool + num_story
  fit <- lm(model ,data = agg2016)
  
  # robsut standard erros
  fit_robust <- coeftest(fit, vcov = vcovHC(fit, type = "HC0"))
  
  # build nice regression table
  export_summs(fit_robust,
               number_format = "%.3f",
               file.name = "2017building.docx", to.file = 'docx')
  
  library(h3jsr)
  # aggregate shapefile
  coords_reduced <- c(agg2016$lat, agg2016$lng)
  h3_index_reduced <- geo_to_h3(coords_reduced)
  sf <- h3_to_geo_boundary_sf(h3_index_reduced)
  
  ct_sf <- st_centroid(st_geometry(sf))
  
  nearest.six <- knearneigh(ct_sf, k = 6) 
  nearest.six2 <- knn2nb(nearest.six) 
  
  # plotting neighbours
  plot(sf)
  plot(nearest.six2, st_geometry(sf), col = 2, add = TRUE)
  
  # SAR Model
  W.listw <- nb2listw(nearest.six2)
  
  # arun model
  fit.sar <- lagsarlm(model, W.listw, data = agg2016, method="eigen", quiet = TRUE)
  summary(fit.sar)
  
  moran.test(hunan$GDPPC, listw = rswm_q, zero.policy = TRUE, na.action = na.omit)
  
  impacts(sar.chi, listw=W)
  
  # plot residuals
  spplot(chi.poly,"chi.sar.res",at=seq(min(chi.poly@data$chi.sar.res,na.rm=TRUE),max(chi.poly@data$chi.sar,na.rm=TRUE), length=12), col.regions=rev(brewer.pal(11,"RdBu")))
  
  
  