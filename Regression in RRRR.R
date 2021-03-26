# regression

data <- read.csv('/Users/kiliangerding/Documents/GitHub/ResearchSeminar/AHS_Total.csv', sep = ',')


# subsetting
rent2015 <- data[(data$RENT != -6 & data$Year == 2015),]
rent2017 <- data[(data$RENT != -6 & data$Year == 2017),]
rent2019 <- data[(data$RENT != -6 & data$Year == 2019),]
   
price2015 <- data[(data$MARKETVAL != -6 & data$Year == 2015),]
price2017 <- data[(data$MARKETVAL != -6 & data$Year == 2017),]
price2019 <- data[(data$MARKETVAL != -6 & data$Year == 2019),]


fit_rent_2015 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2015)
fit_rent_2017 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2017)
fit_rent_2019 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2019)

summary(fit_rent_2015)
plot(fit_rent_2015)
summary(fit_rent_2017)
plot(fit_rent_2017)
summary(fit_rent_2019)
plot(fit_rent_2019)

fit_price_2015 <- lm(MARKETVAL ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2015)
fit_price_2017 <- lm(MARKETVAL ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2017)
fit_price_2019 <- lm(MARKETVAL ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2019)

summary(fit_price_2015)
plot(fit_price_2015)
summary(fit_price_2017)
plot(fit_price_2017)
summary(fit_price_2019)
plot(fit_price_2019)

