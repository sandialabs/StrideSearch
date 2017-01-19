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

data_time_ind = 70
dts = [datetime(2001, 10, 1, 0) + timedelta(days = td) for td in ncd['time'][:]]
plotDate = dts[data_time_ind]

ctr_data = 0.01 * ncd.variables['PS'][data_time_ind][:][:]

fig = plt.figure()
fig.hold(True)
#m = Basemap(projection="ortho", lat_0 = 30, lon_0 = 200, resolution = 'l')
m = Basemap(llcrnrlat = 10.0, llcrnrlon = 120.0, urcrnrlat = 45.0, urcrnrlon = 180.0, projection = 'merc', resolution = 'l')

m.drawcoastlines()
m.drawcountries()
m.fillcontinents(color='coral', lake_color='aqua')
# m.drawmapboundary(fill_color='aqua')
m.drawmeridians(np.arange(120,180,10), labels=[0,0,0,1])
m.drawparallels(np.arange(0,45,10), labels=[1,0,0,0])

#cp = m.contourf(lons, lats, ctr_data, np.arange(-0.003, 0.003, 0.00005), cmap='RdBu_r', latlon=True)
cp = m.contourf(lons, lats, ctr_data, np.arange(960.0, 1030.0), cmap='RdBu', latlon=True)
cb = m.colorbar(cp)
cb.set_label('hPa')

for df in trks:
    trkLon = [df['lon'][j] for j in range(len(df['lon']))]
    trkLat = [df['lat'][i] for i in range(len(df['lat']))]
    m.plot(trkLon, trkLat, linewidth=2, color='k', latlon=True)

plt.title("PS " + str(plotDate))
fig.savefig('ssDemoPlot.png', bbox_inches='tight')




