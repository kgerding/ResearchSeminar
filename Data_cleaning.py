# This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Path
path = os.path.abspath(os.getcwd())
print(path)


# Step 1: Read in Data
ahs_2019 = pd.read_csv(path + "/Data/AHS 2019 National PUF v1.1 CSV/household_national.csv", index_col=0)

# Step 2: get the right values
drop_values = pd.read_excel(path + "/Info on Data/codebook_2019_new.xls")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

# convert to numeric

# drop values
ahs_2019 = ahs_2019[(drop_values.columns) & (ahs_2019.columns)]
ahs_2019 = ahs_2019.replace(r"'", '', regex=True)
ahs_2019 = ahs_2019.apply(pd.to_numeric, errors='coerce')
ahs_2019_both = ahs_2019[(ahs_2019.MARKETVAL & ahs_2019.RENT !=-6)]
ahs_2019_marketval = ahs_2019[(ahs_2019.MARKETVAL != -6)]

# count missing values
counted_ahs_both = ahs_2019_both[ahs_2019_both != -6].count()/ahs_2019_both.shape[0]
counted_ahs_marketval = ahs_2019_marketval[ahs_2019_marketval != -6].count()/ahs_2019_marketval.shape[0]

ahs_2019_both.head()

# Step 3: Match CSMB
longlat_2018 = pd.read_csv(path + "/Data/cbsa-boundaries-2018.csv", sep=';')
longlat_2018.loc[:,"Year"].unique()
longlat_2018.columns