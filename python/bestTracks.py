from pandas import DataFrame, Series
from dateutil.parser import parse
from Event import Event, EventList
from IdentCriteria import TimeCriteria
from Track import TrackList
from glob import glob
from os import chdir
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap

#
#   Define the JTWC ASCII format (http://www.usno.navy.mil/NOOC/nmfc-ph/RSS/jtwc/best_tracks/wpindex.php)
#
headers = ["basin", "cyclone number", "date time group", "blank", "best", "fcst tau", "latitude (0.1deg)", "longitude (0.1deg)", 
           "max. windspeed (kts)", "sea level pressure (hPa)", "dev. stage", "radius category", "wind code", "radius 1 (nm)", "radius 2 (nm)", "radius 3 (nm)", 
           "radius 4 (nm)", "isobar pressure (hPa)", "isobar radius (nm)", "max. wind radius (nm)", "gust speed (kts)", "eye diameter (nm)", 
           "subregion code", "max seas (ft)", "blank", "travel dir. (deg)", "travel speed (kts)", "storm name", "depth code", "seas (ft)", "sea code", "seas1 (nm)",
           "seas2 (nm)", "seas3 (nm)", "seas4 (nm)"] 
#   conversion factors
ktsToMps = 0.514444 # knots to meters per second
nmToKm = 1.852 # nautical mile to kilometer
def getLatitudeFromString(latstr):
    if "N" in latstr:
        return 0.1 * float(latstr.strip("N"))
    elif "S" in latstr:
        return -0.1 * float(latstr.strip("S"))
    else:
        raise ValueError("latstr does not appear to be a latitude value.")

def getLongitudeFromString(lonstr):
    if "E" in lonstr:
        return 0.1 * float(lonstr.strip("E"))
    elif "W" in lonstr:
        return 360.0 - 0.1 * float(lonstr.strip("W"))
    else:
        raise ValueError("lonstr does not appear to be a longitude value.")
#
#   Define the Stride Search event radius
#
maxCycloneRadius = 500.0 # kilometers

# Stride Search source code path
pydir = "/Users/pabosle/StrideSearch/python"
# Data path
datadir =  "/Users/pabosle/Desktop/dataTemp/bestTracks-JTWC"

chdir(datadir)
filenames = glob("./bwp*")

#
#   Step 1: read all JTWC track data points, convert to Stride Search Event type
#       
evList = EventList([])
fileLens = []
dflist = []
for filename in filenames:
    f = open(datadir + "/" + filename, 'r')
    filedict = {}
    lineCount = 0           
    for lineInd, line in enumerate(f):
        lineCount += 1
        linedir = {}
        splitline = line.split(", ")
        #print "len(splitline) = ", len(splitline)
        for i in range(min(len(headers),len(splitline))):
            linedir[headers[i]] = splitline[i].strip()
        linedir["date time group"] = linedir["date time group"] + "00"
        filedict[lineInd] = linedir
        #print linedir
        ll = (getLatitudeFromString(linedir["latitude (0.1deg)"]), getLongitudeFromString(linedir["longitude (0.1deg)"]))
        dt = parse(linedir["date time group"], yearfirst = True)
        dataInd = (filename, "line " + str(lineInd+1))
        linedir['dataIndex'] = dataInd
        ev = Event("min(PSL)", ll, dt, dataInd, {"min" : float(linedir["sea level pressure (hPa)"])})
        relEv = Event("max(windSpd)", ll, dt, dataInd, {'max' : ktsToMps * float(linedir["max. windspeed (kts)"])})
        ev.addRelatedEvent(relEv)
        evList.addEvent(ev)
    fileLens.append(lineCount)
    dflist.append(DataFrame(filedict).T)
    f.close()  

#
#   Step 2: Split the list of events by datetime, as if these data came from a Stride Search spatial search
#
def splitListByDates(eList):
        datedEvLists = []
        dateList = []
        for ev in eList.events:
            if ev.datetime not in dateList:
                dateList.append(ev.datetime)
        for dtg in dateList:
            dtgList = EventList([])
            for ev in eList.events:
                if ev.datetime == dtg:
                    dtgList.addEvent(ev)
            datedEvLists.append(dtgList)
        return dateList, datedEvLists

#
#   Remove duplicate entries of the same storm (see JTWC files -- some track points are listed multiple times in a file to allow for additional data fields)
#
dates, listOfEventLists = splitListByDates(evList)
for el in listOfEventLists:
    el.removeDuplicates(maxCycloneRadius)
    
#
#   Step 3: Build tracks from the list of Stride Search EventLists
#   
minDuration = 24 # hours
maxSpeed = 15.0 # meters per second
timeStepSize = 6.0 # hours
timeCrit = TimeCriteria(minDuration, maxSpeed)

tracks = TrackList(timeCrit, timeStepSize)
tracks.buildTracksFromSpatialResults(listOfEventLists)

#
#   Step 4: Plot output, check for sanity
#
fig = plt.figure()
fig.hold(True)
m = Basemap(llcrnrlat = 0.0, llcrnrlon = 100.0, urcrnrlat = 40.0, urcrnrlon = 180.0, projection = 'merc', resolution = 'l')
m.drawcoastlines()
m.drawcountries()
m.drawmapboundary(fill_color='#99ffff')
m.fillcontinents(color='#cc9966', lake_color='#99ffff') 
for trk in tracks.tracks:
    lldict = trk.getLatLons()
    lats = [lldict[key][0] for key in sorted(lldict)]
    lons = [lldict[key][1] for key in sorted(lldict)]
    if lldict.keys()[0].year == 2006:
        m.plot(lons, lats, linewidth=2, color = 'b', latlon=True)
    else:
        m.plot(lons, lats, linewidth=2, color = 'r', latlon=True)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('WestPac tropical cyclones: 2006 (blue), 2014 (red)')
#plt.show()
fig.savefig('../JTWCTracks.png',bbox_inches='tight')

#
#   Step 5: Verify output against source data to ensure correctness
#
if len(tracks) != len(fileLens):
    print "RuntimeError('Number of tracks does not equal number of files!')"
    for fInd, filename in enumerate(filenames):
        f = open(datadir + "/" + filename, 'r')
        lines = f.readlines()
        f.close()
        if len(tracks[fInd]) != len(lines):
            print "\tline mismatch found: file = %s, lineCount = %d; track = %d has length %d"%(filename, lineCount, fInd, len(tracks[fInd]))
            datesInFile = []
            for line in lines:
                linedict = {}
                splitline = line.split(", ")
                for i in range(min(len(headers),len(splitline))):
                    linedict[headers[i]] = splitline[i].strip()
                linedict["date time group"] = linedict["date time group"] + "00"
                dt = parse(linedict["date time group"], yearfirst = True)
                #print filename, dt
                if dt not in datesInFile:
                    datesInFile.append(dt)
            dateMismatch = len(datesInFile) != len(tracks[fInd])
            if dateMismatch:
                print "\tdates also mismatch: file %s found %d unique dates; track %d has %d entries"%(filename, len(datesInFile), fInd, len(tracks[fInd]))
                break
            else:
                print "\tnumber of dates match, though... proceeding."
        
        
chdir(pydir)
