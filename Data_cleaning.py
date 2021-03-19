#This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Path
path = os.path.abspath(os.getcwd())
print(path)

# Compute the relative file path
# to the given path from the
# the given start directory.
relative_path = os.path.relpath(path, start)

# Print the relative file path
# to the given path from the
# the given start directory.
print(relative_path)

# Step 1: Read in Data
ahs_2019 = pd.read_csv(path + "/Data/AHS 2019 National PUF v1.1 CSV/household_national.csv", index_col=0)

# Step 2: get the right values
drop_values = pd.read_excel(Data/Info_on_Data/codebook_2019.xlsx")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

ahs_2019 = ahs_2019[(drop_values.columns) & (ahs_2019.columns)]
ahs_2019.MARKETVAL[ahs_2019.MARKETVAL != -6]

os.path.relpath()
