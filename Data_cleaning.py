# This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Path
path = os.path.abspath(os.getcwd())
print(path)


# Step 1: Read in Data
ahs_2019 = pd.read_csv(path + "/Data/AHS 2019 National PUF v1.1 CSV/household_national.csv", index_col=0)

# check data types
ahs_2019.dtypes

# Step 2: get the right values
drop_values = pd.read_excel(path + "/Info on Data/codebook_2019.xls")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

ahs_2019 = ahs_2019[(drop_values.columns) & (ahs_2019.columns)]
ahs_2019 = ahs_2019[ahs_2019.MARKETVAL != -6]
ahs_2019 = ahs_2019.replace(r"'", '', regex=True)

# Option 2
#indexNames = ahs_2019[ahs_2019['MARKETVAL'] != -6].index
#ahs_2019.drop(indexNames, inplace=True)


# convert to numeric
cols = ahs_2019.columns
ahs_2019[cols] = ahs_2019[cols].apply(pd.to_numeric, errors='coerce')

# check data types
ahs_2019.dtypes


counted_ahs = ahs_2019[ahs_2019 != -6].count()/ahs_2019.shape[0]

counted_ahs.to_csv('Data/counted_ahs.csv')
