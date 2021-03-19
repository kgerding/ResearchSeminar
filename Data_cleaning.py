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

# Step 2: get the right values
drop_values = pd.read_excel("/Users/kiliangerding/OneDrive - Universität St.Gallen/MBF_2_Semester/Courses/Research Seminar/Data/Info_on_Data/codebook_2019.xlsx")
drop_values = drop_values[drop_values.Include == 'x']
drop_values = pd.DataFrame(columns=drop_values['Variable'].values)

ahs_2019 = ahs_2019[(drop_values.columns) & (ahs_2019.columns)]
ahs_2019.MARKETVAL[ahs_2019.MARKETVAL != -6]