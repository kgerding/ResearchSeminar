# This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Path
path = os.path.abspath(os.getcwd())
print(path)

##### 2015 ##### ---------------------------------------------------------

# Step 1: Read in Data
ahs_2015 = pd.read_csv(
    path + "/Data/AHS 2015 National PUF v3.1 CSV/household.csv", index_col=0)

# Step 2: get the right values
drop_values = pd.read_excel(path + "/Info on Data/codebook_2019_new.xls")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

# drop values
ahs_2015 = ahs_2015[(drop_values.columns) & (ahs_2015.columns)]
ahs_2015 = ahs_2015.replace(r"'", '', regex=True)
ahs_2015 = ahs_2015.apply(pd.to_numeric, errors='coerce')
ahs_2015_both = ahs_2015[(ahs_2015.MARKETVAL & ahs_2015.RENT != -6)]
ahs_2015_marketval = ahs_2015[(ahs_2015.MARKETVAL != -6)]

# count missing values
counted_ahs_both = ahs_2015_both[ahs_2015_both != -
                                 6].count()/ahs_2015_both.shape[0]
counted_ahs_marketval = ahs_2015_marketval[ahs_2015_marketval != -6].count(
)/ahs_2015_marketval.shape[0]

ahs_2015_both


##### 2017 ##### ---------------------------------------------------------
# Step 1: Read in Data
ahs_2017 = pd.read_csv(
    path + "/Data/AHS 2017 National PUF v3.1 CSV/household.csv", index_col=0)

# Step 2: get the right values
drop_values = pd.read_excel(path + "/Info on Data/codebook_2019_new.xls")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

# convert to numeric

# drop values
ahs_2017 = ahs_2017[(drop_values.columns) & (ahs_2017.columns)]
ahs_2017 = ahs_2017.replace(r"'", '', regex=True)
ahs_2017 = ahs_2017.apply(pd.to_numeric, errors='coerce')
ahs_2017_both = ahs_2017[(ahs_2017.MARKETVAL & ahs_2017.RENT != -6)]
ahs_2017_marketval = ahs_2017[(ahs_2017.MARKETVAL != -6)]

# count missing values
counted_ahs_both = ahs_2017_both[ahs_2017_both != -
                                 6].count()/ahs_2017_both.shape[0]
counted_ahs_marketval = ahs_2017_marketval[ahs_2017_marketval != -6].count(
)/ahs_2017_marketval.shape[0]

ahs_2017_both

##### 2019 ##### ---------------------------------------------------------

# Step 1: Read in Data
ahs_2019 = pd.read_csv(
    path + "/Data/AHS 2019 National PUF v1.1 CSV/household_national.csv", index_col=0)

# Step 2: get the right values
drop_values = pd.read_excel(path + "/Info on Data/codebook_2019_new.xls")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

# drop values
ahs_2019 = ahs_2019[(drop_values.columns) & (ahs_2019.columns)]
ahs_2019 = ahs_2019.replace(r"'", '', regex=True)
ahs_2019 = ahs_2019.apply(pd.to_numeric, errors='coerce')
ahs_2019_both = ahs_2019[(ahs_2019.MARKETVAL & ahs_2019.RENT != -6)]
ahs_2019_marketval = ahs_2019[(ahs_2019.MARKETVAL != -6)]

# count missing values
counted_ahs_both = ahs_2019_both[ahs_2019_both != -
                                 6].count()/ahs_2019_both.shape[0]
counted_ahs_marketval = ahs_2019_marketval[ahs_2019_marketval != -6].count(
)/ahs_2019_marketval.shape[0]

ahs_2019_both

##### COMPARISON ##### ---------------------------------------------------------
files = [ahs_2015_both, ahs_2017_both, ahs_2019_both]

# reset index for all files
for i in files:
    i.reset_index(inplace=True)
    i.loc[:, 'CONTROL'] = i.loc[:, 'CONTROL'].replace(r"'", '', regex=True)
    i.loc[:, 'CONTROL'] = i.loc[:, 'CONTROL'].apply(pd.to_numeric, errors='coerce')
    print(i.dtypes)

# set up an empty dataframe
columns = ['MARKETVAL', 'YRBUILT', 'TOTROOMS', 'RENT', 'KITCHENS', 'BATHROOMS', 'BEDROOMS']
index = (1,2,3)
df = pd.DataFrame(index = index, columns = columns)
df

#check for control variable 1 if there are any changes
x = 11000001
for col in columns:
    df.loc[1, col] = (ahs_2015_both.loc[ahs_2015_both['CONTROL'] == x])[col][0]
    df.loc[2, col] = (ahs_2017_both.loc[ahs_2017_both['CONTROL'] == x])[col][0]
    df.loc[3, col] = (ahs_2019_both.loc[ahs_2019_both['CONTROL'] == x])[col][0]

##### CONCATIANTE ##### ---------------------------------------------------------
# get all the same columns for concat
ahs_2015_both = ahs_2015_both[(ahs_2015_both.columns) & (ahs_2017_both.columns) & (ahs_2019_both.columns)]
ahs_2017_both = ahs_2017_both[(ahs_2015_both.columns) & (ahs_2017_both.columns) & (ahs_2019_both.columns)]
ahs_2019_both = ahs_2019_both[(ahs_2015_both.columns) & (ahs_2017_both.columns) & (ahs_2019_both.columns)]

# add column with respective year
ahs_2015_both['Year'] = 2015
ahs_2017_both['Year'] = 2017
ahs_2019_both['Year'] = 2019

# concat dataframes in long-format
ahs_total = pd.concat([ahs_2015_both, ahs_2017_both, ahs_2019_both])