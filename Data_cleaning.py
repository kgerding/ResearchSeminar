#This file is for data cleaning

import pandas as pd

# Step 1: Read in Data
path = "/Users/kiliangerding/OneDrive - Universität St.Gallen/MBF_2_Semester/Courses/Research Seminar/Data/AHS 2015 National PUF v3.1 CSV/household.csv"
ahs_2019 =  pd.read_csv(path, index_col=0)
ahs_2019
