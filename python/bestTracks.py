from pandas import DataFrame
from dateutil.parser import parse
from Event import Event, EventList, print_copyright
from IdentCriteria import TimeCriteria
from Track import TrackList
from glob import glob
from os import chdir, getcwd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from mpl_toolkits.basemap import Basemap
from collections import OrderedDict

print_copyright()
print "This program provides and end-to-end test of the Track and TrackList classes."
print "1. Read in known tropical cyclone tracks from JTWC Best Track data"
print "2. Convert the JTWC data into Stride Search Events and Event Lists"
print "3. Restructure the Event Lists to look like the output of a Stride Search spatial detection"
print "4. Build tracks from the Stride Search imitation data"
print "5. Check that the Stride Search tracks exactly reproduce the JTWC tracks"
print "6. Just for kicks, make a plot too.\n"

verbose = False

#
#   Define the JTWC ASCII format (http://www.usno.navy.mil/NOOC/nmfc-ph/RSS/jtwc/best_tracks/wpindex.php)
#
headers = ["basin", "cyclone number", "date time group", "blank", "best", "fcst tau", "latitude (0.1deg)", "longitude (0.1deg)", 
           "max. windspeed (kts)", "sea level pressure (hPa)", "dev. stage", "radius category", "wind code", "radius 1 (nm)", "radius 2 (nm)", "radius 3 (nm)", 
           "radius 4 (nm)", "isobar pressure (hPa)", "isobar radius (nm)", "max. wind radius (nm)", "gust speed (kts)", "eye diameter (nm)", 
           "subregion code", "max seas (ft)", "blank", "travel dir. (deg)", "travel speed (kts)", "storm name", "depth code", "seas (ft)", "sea code", "seas1 (nm)",
           "seas2 (nm)", "seas3 (nm)", "seas4 (nm)"]
tsHours = 6.0           
             
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
pydir = getcwd()
# Data path
datadir =  "../testData"

chdir(datadir)
filenames = glob("./bwp*")

#
#   Step 1: read all JTWC track data points, convert to Stride Search Event type
#       Store original data in DataFrames for validation
#       
evList = EventList([])
fileLens = []
fileDateList = []
jtwcDfDict = {}
tsRecordHours = [0, 6, 12, 18]
for fInd, filename in enumerate(filenames):
    f = open(datadir + "/" + filename, 'r')
    filedict = {}
    lineCount = 0
    fileEventCount = 0
    fileEvents = EventList([])
    fileDateList.append([])
    for lineInd, line in enumerate(f):
        lineCount += 1
        linedir = {}
        splitline = line.split(", ")
        for i in range(min(len(headers),len(splitline))):
            linedir[headers[i]] = splitline[i].strip()
        linedir["date time group"] = linedir["date time group"] + "00"
        ll = (getLatitudeFromString(linedir["latitude (0.1deg)"]), getLongitudeFromString(linedir["longitude (0.1deg)"]))
        dt = parse(linedir["date time group"], yearfirst = True)
        if dt.hour in tsRecordHours:
            if dt not in fileDateList[fInd]:
                fileDateList[fInd].append(dt)
            dataInd = (filename, "line " + str(lineInd+1))
            linedir['dataIndex'] = dataInd
            ev = Event("min(PSL)", ll, dt, dataInd, {"min" : float(linedir["sea level pressure (hPa)"])})
            relEv = Event("max(windSpd)", ll, dt, dataInd, {'max' : ktsToMps * float(linedir["max. windspeed (kts)"])})
            ev.addRelatedEvent(relEv)
            fileEvents.addEvent(ev)
            fileEventCount += 1
            filedict[dt] = linedir
    fileEvents.removeDuplicates(maxCycloneRadius)
    evList.extend(fileEvents)
    fileLens.append(lineCount)
    jtwcDfDict[fileDateList[fInd][0]] = DataFrame(filedict).T
    f.close()  

#
#   Step 2: Split the list of events by datetime, as if these data came from a Stride Search spatial search
#
def splitListByDates(eList):
        datedEvLists = OrderedDict([])
        dateList = []
        for ev in eList.events:
            if ev.datetime not in dateList:
                dateList.append(ev.datetime)
        dateList.sort()
        for dtg in dateList:
            dtgList = EventList([])
            for ev in eList.events:
                if ev.datetime == dtg:
                    dtgList.addEvent(ev)
            datedEvLists[dtg] = dtgList
        return dateList, datedEvLists

##
##   Remove duplicate entries of the same storm (see JTWC files -- some track points are listed multiple times in a file to allow for additional data fields)
##
dates, eventListsByDate = splitListByDates(evList)
es = 0
for evL in eventListsByDate:
    es += len(eventListsByDate[evL])
if verbose:    
    print "from ", sum(fileLens), " lines in data files, found ", len(dates), " distinct dates and ", es, " distinct events."
    
#
#   Step 3: Build tracks from the list of Stride Search EventLists
#   
minDuration = 12 # hours
maxSpeed = 30.0 # meters per second
timeStepSize = tsHours # hours
timeCrit = TimeCriteria(minDuration, maxSpeed)

tracks = TrackList(timeCrit, timeStepSize)
tracks.buildTracksFromSpatialResults(eventListsByDate)
if verbose:
    tracks.printInfo()

#headers = ["basin", "cyclone number", "date time group", "blank", "best", "fcst tau", "latitude (0.1deg)", "longitude (0.1deg)", 
#           "max. windspeed (kts)", "sea level pressure (hPa)", "dev. stage", "radius category", "wind code", "radius 1 (nm)", "radius 2 (nm)", "radius 3 (nm)", 
#           "radius 4 (nm)", "isobar pressure (hPa)", "isobar radius (nm)", "max. wind radius (nm)", "gust speed (kts)", "eye diameter (nm)", 
#           "subregion code", "max seas (ft)", "blank", "travel dir. (deg)", "travel speed (kts)", "storm name", "depth code", "seas (ft)", "sea code", "seas1 (nm)",
#           "seas2 (nm)", "seas3 (nm)", "seas4 (nm)"] 

def getDataDictFromTrack(trk):
    trkDict = {}
    for ev in trk.events:
        fs = ev.dataIndex[0]
        dtstr = '%04d%02d%02d%02d00'%(ev.datetime.year, ev.datetime.month, ev.datetime.day, ev.datetime.hour)
        if ev.latLon[0] >= 0.0:
            latstr = "%dN"%(int(ev.latLon[0] * 10.0))
        else:
            latstr = "%dS"%(int(ev.latLon[0] * 10.0))
        if ev.latLon[1] > 180.0:
            lonstr = "%dW"%(int(10.0 * (180.0 - ev.latLon[1])))
        else:
            lonstr = "%dE"%(int(10.0 * ev.latLon[1]))
        if ev.desc == 'min(PSL)':
            minPSL = str(int(ev.vals['min']))
        else:
            minPSL = str(int(ev.related[0].vals['min']))
        if ev.desc == 'max(windSpd)':
            maxWind = str(int(round(ev.vals['max'] / ktsToMps)))
        else:
            maxWind = str(int(round(ev.related[0].vals['max'] / ktsToMps)))
        evDict = {'basin' : 'WP', 'cyclone number' : fs[5:7], 'date time group' : dtstr, 'blank': ' ', 'best' : 'BEST', 'fcst tau' : '0', 
            "latitude (0.1deg)" : latstr, 'longitude (0.1deg)':lonstr, 'max. windspeed (kts)':maxWind, 'sea level pressure (hPa)' : minPSL}
        trkDict[ev.datetime] = evDict
    return trkDict

ssDfDict = {}
for trk in tracks.tracks:
    tDict = getDataDictFromTrack(trk)
    startDate = min(tDict.keys())
    ssDfDict[startDate] = DataFrame(tDict).T

#
#   Step 4: Verify output against source data to ensure correctness
#
passCounter = 0
testCounter = 1
if len(tracks) == len(filenames):
    if verbose:
        print "Test PASSED: number of tracks == number of JTWC files"
    passCounter += 1
else:
    raise RuntimeError('Number of tracks %d does not equal number of files %d!'%(len(tracks), len(fileLens)))
checkVariables = ["date time group", "latitude (0.1deg)", "longitude (0.1deg)", "sea level pressure (hPa)", "max. windspeed (kts)"]
trackCounter = 0
for dt in jtwcDfDict:
    df0 = jtwcDfDict[dt]
    df1 = ssDfDict[dt]
    trackCounter += 1
    for var in checkVariables:
       testCounter += 1
       eqCheck = df0[var] == df1[var]
       falseCount = 0
       for bl in eqCheck:
           if bl == False:
               falseCount += 1
       if falseCount > 0:
           raise RuntimeError("data frame mismatch, date %s, variable %s"%(dt, var))
       else:
           passCounter +=1
           if verbose:
               print "Test PASSED: Stride Search %s data matches JTWC %s data"%(var, var)
if passCounter == testCounter:
    print "SUCCESS: all tests passed (%d/%d)."%(passCounter, testCounter)
else:
    print "ERROR: %d tests Failed out of %d"%(testCounter - passCounter, testCounter)
                
               
    
    
#
#   Step 5: Plot output
#
fig = plt.figure()
fig.hold(True)
m = Basemap(llcrnrlat = 0.0, llcrnrlon = 100.0, urcrnrlat = 40.0, urcrnrlon = 180.0, projection = 'merc', resolution = 'l')
m.drawcoastlines()
m.drawcountries()
m.drawmapboundary(fill_color='#99ffff')
m.fillcontinents(color='#cc9966', lake_color='#99ffff') 
markers = Line2D.filled_markers
mkrInd = 0
for trk in tracks.tracks:
    lldict = trk.getLatLons()
    lats = [lldict[key][0] for key in sorted(lldict)]
    lons = [lldict[key][1] for key in sorted(lldict)]
    if lldict.keys()[0].year == 2013:
        m.plot(lons, lats, linewidth=2, color = 'b', latlon=True)
    elif lldict.keys()[0].year == 2014:
        m.plot(lons, lats, linewidth=2, color = 'r', latlon=True)
        #mrk = markers[mkrInd]
        #m.plot(lons, lats, linewidth=2, color = 'r', marker = mrk, latlon=True)
        #for key in sorted(lldict):
        #    if key.hour == 0:
        #        x, y = m(lldict[key][1],lldict[key][0])
        #        plt.text(x, y, str(key.month)+'-'+str(key.day))
        #mkrInd += 1
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('WestPac tropical cyclones: 2013 (blue), 2014 (red)')
#plt.show()
fig.savefig('../JTWCTracks.png',bbox_inches='tight')        
        
chdir(pydir)

    