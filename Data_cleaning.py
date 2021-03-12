#This file is for data cleaning
import os
import pandas as pd
import numpy as np

# Step 1: Read in Data
# path Kilian
# path = "/Users/kiliangerding/OneDrive - Universität St.Gallen/MBF_2_Semester/Courses/Research Seminar/Data/AHS 2015 National PUF v3.1 CSV/household.csv"

# path Tim
# path

ahs_2019 =  pd.read_csv(path, index_col=0)
ahs_2019

# Step 2: get the right values
path2 = "/"

# Path
path = "/home / User / Desktop / file.txt"

# Path of Start directory
start = "/home / User"

# Compute the relative file path
# to the given path from the
# the given start directory.
relative_path = os.path.relpath(path, start)

# Print the relative file path
# to the given path from the
# the given start directory.
print(relative_path)
