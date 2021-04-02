# Rsearch Seminar: Real Estate  -------------------------------------------
# Authors: Tim Graf, Kilian Gerding

# Packages used -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)
library(spdep)




# Read in Data ------------------------------------------------------------

# prices from Zillow transactions 2016 and 2017
prices2016 <- read.csv('./Data/properties_2016.csv', sep = ',')
prices2017 <- read.csv('./Data/properties_2017.csv', sep = ',')

# Data Inspection ---------------------------------------------------------

# Step 0: Rename the Data

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

# Step 1: Eliminate columns with more than 20% NAs

# quick plot
count_nas <- colSums(is.na(p2016))/nrow(p2016)
sorted <- rev(sort(count_nas))
barplot(sorted, cex.names = 0.5, las = 2)
abline(v=35, col="red")

#delete columns
#p2016 <- p2016[, colSums(is.na(p2016)/nrow(p2016)) < 0.2]
missing_values <- p2016 %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct < 0.20)
features <- good_features$feature
p2016 <- p2016 %>% select(features)


# select hedonics
hedonics <- c('id_parcel','num_bathroom','num_bedroom','area_live_finished',
              'flag_tub_or_spa','loc_latitude','loc_longitude','area_lot','type_zoning_landuse_county',
              'type_zoning_landuse','loc_zip','loc_county', 'year_built', 'flag_fireplace', 'num_tax_building',
              'num_tax_total', 'num_tax_land')
p2016 <- p2016[, hedonics ]

# transform dummies and factors
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa == 'true'] <- 1
p2016$flag_tub_or_spa[p2016$flag_tub_or_spa != 'true'] <- 0
p2016$flag_fireplace[p2016$flag_fireplace == 'true'] <- 1
p2016$flag_fireplace[p2016$flag_fireplace != 'true'] <- 0
p2016$flag_tub_or_spa <- as.numeric(p2016$flag_tub_or_spa)
p2016$flag_fireplace <- as.numeric(p2016$flag_fireplace)



# clean house prices
hist(p2016$num_tax_total[p2016$num_tax_total < 1000000], breaks = 100)
p2016 <- p2016[p2016$num_tax_total >= 50000,]

summary(p2016) 


# select only relevant
test <- na.omit(p2016)


# lm 
hedonic <- lm(num_tax_building ~ num_bathroom + num_bedroom + area_live_finished + area_lot + year_built, data = p2016)
summary(hedonic)



