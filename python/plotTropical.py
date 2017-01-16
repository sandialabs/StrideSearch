from Event import print_copyright
from Track import TrackList
from datetime import datetime, timedelta
from dateutil.parser import parse
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from mpl_toolkits.basemap import Basemap
from netCDF4 import Dataset

#print_copyright()

dataDir = '/Users/pabosle/Desktop/dataTemp'

datafilename = dataDir + '/' + 'f1850c5_ne240_rel06.cam.h2.0004-07-18-00000.nc'
ss_trackfilename = dataDir + '/' + 'ssResults/tropicalTracks_westpac_JulyYear4.h5'

dstore = pd.HDFStore(ss_trackfilename)

trks = [dstore['trk5'], dstore['trk6'], dstore['trk7']]



ncd = Dataset(datafilename, "r")
lons, lats = np.meshgrid(ncd.variables['lon'][:], ncd.variables['lat'][:])

data_time_ind = 59
dts = [datetime(2001, 10, 1, 0) + timedelta(days = td) for td in ncd['time'][:]]
plotDate = dts[data_time_ind]

ctr_data = ncd.variables['VOR850'][data_time_ind][:][:]

fig = plt.figure()
fig.hold(True)
m = Basemap(projection="ortho", lat_0 = 30, lon_0 = 200, resolution = 'l')
m.drawcoastlines()
m.drawcountries()
m.fillcontinents(color='coral', lake_color='aqua')
m.drawmapboundary(fill_color='aqua')
m.drawmeridians(np.arange(0,360,30))
m.drawparallels(np.arange(-90,90,30))

cp = m.contourf(lons, lats, ctr_data, latlon=True)

for df in trks:
    trkLon = [df['lon'][j] for j in range(len(df['lon']))]
    trkLat = [df['lat'][i] for i in range(len(df['lat']))]
    m.plot(trkLon, trkLat, linewidth=2, color='r', latlon=True)


plt.title(str(plotDate))
plt.show()



