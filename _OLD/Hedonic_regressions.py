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

# create linear regression
x = ahs.RATINGHS
z = ahs.YRBUILT
y = ahs.MARKETVAL

x_variables = ['YRBUILT', 'TOTROOMS', 'KITCHENS', 'BATHROOMS', 'BEDROOMS']

# Note the difference in argument order
model = sm.OLS(ahs['MARKETVAL'], ahs[x_variables]).fit()

# Print out the statistics
model.summary()