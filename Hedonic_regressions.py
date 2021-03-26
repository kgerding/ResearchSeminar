# This file is for data cleaning
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn import datasets, linear_model
from scipy import stats


# Path
path = os.path.abspath(os.getcwd())
print(path)

# Step 1: Read in Data
ahs = pd.read_csv(
    path + "/AHS_Total.csv", index_col=0)

# create linear regression
regr = linear_model.LinearRegression()

#linear regression
regr.fit(ahs.RATINGHS, ahs.MARKETVAL)

