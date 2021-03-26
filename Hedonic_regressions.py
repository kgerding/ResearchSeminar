# This file is for ahs cleaning
import statsmodels.api as sm
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn import ahssets, linear_model
from scipy import stats
import statsmodels.api as sm


# Path
path = os.path.abspath(os.getcwd())
print(path)

# Step 1: Read in ahs
ahs = pd.read_csv(
    path + "/AHS_Total.csv", index_col=0)


# subsetting
rent2015 = ahs[(ahs$RENT != -6 & ahs$Year == 2015),]
rent2017 = ahs[(ahs$RENT != -6 & ahs$Year == 2017),]
rent2019 = ahs[(ahs$RENT != -6 & ahs$Year == 2019),]
   
price2015 = ahs[(ahs$MARKETVAL != -6 & ahs$Year == 2015),]
price2017 = ahs[(ahs$MARKETVAL != -6 & ahs$Year == 2017),]
price2019 = ahs[(ahs$MARKETVAL != -6 & ahs$Year == 2019),]

# create linear regression
x = ahs.RATINGHS
z = ahs.YRBUILT
y = ahs.MARKETVAL

x_variables = ['YRBUILT', 'TOTROOMS', 'KITCHENS', 'BATHROOMS', 'BEDROOMS']

# Note the difference in argument order
model = sm.OLS(ahs['MARKETVAL'], ahs[x_variables]).fit()

# Print out the statistics
model.summary()