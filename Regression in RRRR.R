# regression

data <- read.csv('./AHS_Total.csv', sep = ',')


# subsetting
rent2015 <- data[(data$RENT != -6 & data$Year == 2015),]
rent2017 <- data[(data$RENT != -6 & data$Year == 2017),]
rent2019 <- data[(data$RENT != -6 & data$Year == 2019),]
   
price2015 <- data[(data$MARKETVAL != -6 & data$Year == 2015),]
price2017 <- data[(data$MARKETVAL != -6 & data$Year == 2017),]
price2019 <- data[(data$MARKETVAL != -6 & data$Year == 2019),]


# Standard linear regression ---------------------------------------------
fit_rent_2015 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2015)
fit_rent_2017 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2017)
fit_rent_2019 <- lm(RENT ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = rent2019)

summary(fit_rent_2015)
#plot(fit_rent_2015)

m <- floor(0.75*length(resid(fit_rent_2015))^(1/3)) #rule of thumb for nb of lags
#coeftest function (from the lmtest package), where we can specify our var-cov matrix (vcov):
fit.rob <- coeftest(fit_rent_2015, vcov = NeweyWest(fit_rent_2015, lag=m, prewhite = F, adjust = T)) 
fit.rob


summary(fit_rent_2017)
#plot(fit_rent_2017)
summary(fit_rent_2019)
#plot(fit_rent_2019)

fit_price_2015 <- lm(log(MARKETVAL) ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2015)
fit_price_2017 <- lm(log(MARKETVAL) ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2017)
fit_price_2019 <- lm(log(MARKETVAL) ~ BEDROOMS + BATHROOMS + KITCHENS + YRBUILT + GARAGE + STORIES,data = price2019)

# get a sense of the data
hist(log(price2015$MARKETVAL), breaks = 500, xlim = c(0, 17))
summary(fit_price_2015)
summary(fit_price_2017)
summary(fit_price_2019)

#plot the marketval against marketprice
plot(price2015$MARKETVAL, price2015$UNITSIZE)

# find unique values for given columns
columns = c('BEDROOMS' ,'BATHROOMS' ,'KITCHENS' ,'YRBUILT' ,'GARAGE' , 'UNITSIZE')
for (col in columns) {
  print(unique(price2015[col]))
}

#check how many marketvalues are below 500
price2015$MARKETVAL[price2015$MARKETVAL < 500]
length(price2015$MARKETVAL[price2015$MARKETVAL < 500])


#check the summary of the market valuations
summary(price2015$MARKETVAL)
summary(price2017$MARKETVAL)
summary(price2019$MARKETVAL)


# Ridge Regresion -----------------------------------
library(glmnet)
library (dplyr)
#install.packages("MASS")
library(MASS)

x <- price2015 %>% select(BEDROOMS, BATHROOMS, KITCHENS, YRBUILT, GARAGE, STORIES) %>% data.matrix()
scale(price2015$BEDROOMS)

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, price2015$MARKETVAL, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg$beta)

coef(ridge_reg)


#cross validation
cv_fit <- cv.glmnet(x, price2015$MARKETVAL, alpha = 0, lambda = lambdas)
# plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
fit <- cv_fit$glmnet.fit
summary(fit)

y_predicted <- predict(fit, s = opt_lambda, newx = x)
y = price2015$MARKETVAL



