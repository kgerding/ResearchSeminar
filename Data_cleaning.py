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


# Step 3: Match CSMB
longlat_2018 = pd.read_csv(path + "/Data/cbsa-boundaries-2018.csv", sep=';')
longlat_2018 = longlat_2018.loc[:,["CBSAFP","Geo Point"]]
longlat_2018[['long', 'lat']] = longlat_2018['Geo Point'].str.split(',', expand=True)
longlat_2018 = longlat_2018.drop('Geo Point', 1)
longlat_2018. columns = ['OMB13CBSA', 'longitude', 'latitude']


# Step 4: Merge with ahs data
newdf = ahs_2019.merge(longlat_2018, on=['OMB13CBSA'])

import matplotlib 
import mpl_toolkits
from mpl_toolkits.basemap import Basemap

# 1. Draw the map background
fig = plt.figure(figsize=(8, 8))
m = Basemap(projection='lcc', resolution='h', 
            lat_0=37.5, lon_0=-119,
            width=1E6, height=1.2E6)
m.shadedrelief()
m.drawcoastlines(color='gray')
m.drawcountries(color='gray')
m.drawstates(color='gray')

# 2. scatter city data, with color reflecting population
# and size reflecting area
m.scatter(lon, lat, latlon=True,
          c=np.log10(population), s=area,
          cmap='Reds', alpha=0.5)

# 3. create colorbar and legend
plt.colorbar(label=r'$\log_{10}({\rm population})$')
plt.clim(3, 7)

# make legend with dummy points
for a in [100, 300, 500]:
    plt.scatter([], [], c='k', alpha=0.5, s=a,
                label=str(a) + ' km$^2$')
plt.legend(scatterpoints=1, frameon=False,
           labelspacing=1, loc='lower left');