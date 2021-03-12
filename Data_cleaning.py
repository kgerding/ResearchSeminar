#This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Path
path = "/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository/Data"

# Path of Start directory
start = "/Users/tgraf/Google Drive/Uni SG/Master/Research Seminar /Repository/"

# Compute the relative file path
# to the given path from the
# the given start directory.
relative_path = os.path.relpath(path, start)

# Print the relative file path
# to the given path from the
# the given start directory.
print(relative_path)

# Step 1: Read in Data
ahs_2019 = pd.read_csv("Data/AHS 2019 National PUF v1.1 CSV/household_national.csv", index_col=0)

ahs_2019

# Step 2: get the right values
drop_values = pd.read_csv("Data/codebook_2019.csv")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = drop_values[['Variable']]


