"""
Stride Tropical Cyclone Search driver.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
from glob import glob
from os import chdir, remove

from SectorList import SectorListLatLon
from Data import LatLonSearchData
from Event import EventList, print_copyright, dtgString
from IdentCriteria import MaxSignedCriterion, MinCriterion, CollocationCriterion, VerticalAverageVariationCriterion
from IdentCriteria import MaxMagnitudeCriterion, TimeCriteria
from Track import TrackList
from pandas import DataFrame, Series, HDFStore
from collections import OrderedDict
import time

print_copyright()
#
#   USER: DEFINE DESIRED OUTPUT
#
verbose = True
write_to_HDF = True
hdfFile = 'tropicalTracks_westpac_JulyYear4.h5'
save_space_output = True
write_space_output = True
spaceFile = 'strideSearch_spatialResults.h5'

#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

if write_to_HDF:
    print "Final output will be saved to file: " + hdfFile
    try: 
        remove(hdfFile)
    except:
        pass
if save_space_output:
    print "\nSpatial search output will be saved to file: " + spaceFile + "\n"
    try:
        remove(spaceFile)
    except:
        pass
chdir(dataPath) 
ncFiles = glob("*0004-07*.nc")
#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = -40.0
northBnd = 40.0
westBnd = 100.0
eastBnd = 270.0
radius = 450.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
vorMax_to_PSLMin_dist = 450.0
tempMax_to_PSLMin_dist = 225.0

crit1 = MaxSignedCriterion('VOR850', 8.5e-4)
crit2 = MinCriterion('PSL', 99000.0)
crit3 = VerticalAverageVariationCriterion('T500', 'T200', 2.0)
crit4 = MaxMagnitudeCriterion('UBOT', 'VBOT', 5.0)
spatialCriteria = [crit1, crit2, crit3, crit4]
print 'identification spatialCriteria set:'
for crit in spatialCriteria:
    print "\t" + crit.infoString()
print ' '

ccrit1 = CollocationCriterion(crit1.returnEventType(), crit2.returnEventType(), vorMax_to_PSLMin_dist)
ccrit2 = CollocationCriterion(crit1.returnEventType(), crit3.returnEventType(), tempMax_to_PSLMin_dist)
ccrit3 = CollocationCriterion(crit2.returnEventType(), crit4.returnEventType(), radius)
collocCriteria = [ccrit1, ccrit2, ccrit3]
print 'collocation criteria set:'
for crit in collocCriteria:
    print "\t" + crit.infoString()
print ' '

minDuration = 12.0 # hours
maxSpeed = 15.0 # m/s
timeCrit = TimeCriteria(minDuration, maxSpeed)
print 'time criteria set:'
timeCrit.printInfo()
print ' '

### PROGRAM START ###
chdir(dataPath)
#
#   Build search sectors
#
sl = SectorListLatLon(southBnd, northBnd, westBnd, eastBnd, radius)
print 'sector list built:'
sl.printInfo()
print ' '

#
#   Setup input data readers
#
ssdata = LatLonSearchData(ncFiles[0])
ssdata.initTime()
ssdata.initDimensions()
datastarttime = ssdata.datetimes[0]
timestep_size = ssdata.datetimes[1] - ssdata.datetimes[0]
timestep_size = timestep_size.seconds / 3600.0
print "timestep size:", timestep_size, 'hours'
print ' '
#
#   Give sectors info about the grid
#
gridDesc = [ssdata.nLat, ssdata.nLon]
sl.setupSectorsForData(gridDesc)
#
#   Stride Search Part I: Spatial Search
#   Loop over files (NOTE: this loop is embarassingly parallel)
#
print "************************************"
print "STRIDE SEARCH STEP 1: Spatial search"
print "************************************"
spStart = time.time()
L = OrderedDict([])
nTotalTimesteps = 0
nFiles = len(ncFiles)
for fi, dfile in enumerate(ncFiles):
    fileStart = time.time()
    if verbose:
        print 'reading file: %s ...'%(dfile)
    ssdata.updateSourceFile(dfile)
    ssdata.initTime()
    if verbose:
        ssdata.printInfo()
    nTotalTimesteps += ssdata.nTimesteps
    #
    #   Loop over time steps (NOTE: this loop is embarassingly parallel)
    #
    for time_ind in range(5): #range(ssdata.nTimesteps):
        evList = EventList([]) # must be re-initialized with an empty list!
        ssdata.loadFileDataForCriteriaAtTimestep(spatialCriteria, time_ind)
        dt = ssdata.datetimes[time_ind]
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        if verbose:
            statstr = '\tfile %d of %d, timestep %d of %d: '%(fi+1, nFiles, time_ind+1, ssdata.nTimesteps)
        for k in range(sl.nSectors):
            sec = sl.findSectorInData(k, ssdata)
            for crit in spatialCriteria:
                wspc = ssdata.getSectorWorkingDataForCriterion(crit, sec)
                if crit.evaluate(sec, wspc):
                    ev = crit.returnEvent(sec, wspc, dt)
                    evList.addEvent(ev)
        evList.removeDuplicates(radius)
        evList.consolidateRelated(radius)
        evList.requireCollocation(collocCriteria)
        if verbose:
            print statstr + 'found ', len(evList), ' events matching spatial criteria.'
        L[dt] = evList
    fileEnd = time.time()
    if verbose:
        print "file complete. elapsed time %f s"%(fileEnd - fileStart)

print 'Spatial search complete: %d time steps searched; elapsed time = %f minutes\n'%(len(L), (time.time() - spStart) / 60.0)

if save_space_output:
    spData = {}
    for dt in L:
        dtDict = {}
        for i, ev in enumerate(L[dt].events):
            #dtDict[i] = ev.convertToSeries()
            dtDict[i] = ev.convertToDataFrame()
        spData[dt] = dtDict
    for dt in spData:
        for evInd in spData[dt]:
            spData[dt][evInd].to_hdf(spaceFile, dtgString(dt)+"_"+str(evInd))
#     spDF.to_hdf(spaceFile, 'spcOutput', mode = 'w', format = 'table')
        
    
          

#
#   Stride Search Part II: Temporal correlation
#   NOTE: This section is serial
#
print "**********************************"
print "STRIDE SEARCH STEP 2: Build tracks"
print "**********************************"

tracks = TrackList(timeCrit, timestep_size)
tracks.buildTracksFromSpatialResults(L)
if verbose:
    tracks.printInfo()
    
def makeDictFromTrack(trk):
    trkDict = {}
    for ev in trk.events:
        evDict = {}
        dtstr = 'dtg%04d%02d%02d%02d00'%(ev.datetime.year, ev.datetime.month, ev.datetime.day, ev.datetime.hour)
        evDict['datetime'] = ev.datetime
        evDict['latlon'] = ev.latLon
        if "max(VOR" in ev.desc:
            evDict['VOR850'] = ev.vals['max']
        elif "min(PSL)" == ev.desc:
            evDict['PSL'] = ev.vals['min']
        elif "max(sqrt(" in ev.desc:
            evDict['windspd'] = ev.vals['max']
        for relEv in ev.related:
            if "max(VOR" in relEv.desc:
                evDict['VOR850'] = relEv.vals['max']
            elif "min(PSL)" == relEv.desc:
                evDict['PSL'] = relEv.vals['min']
            elif "max(sqrt(" in relEv.desc:
                evDict['windspd'] = relEv.vals['max']
        trkDict[dtstr] = evDict
    return trkDict

ssDataDict = {}
trkNo = 0
for trk in tracks.tracks:
    tDict = makeDictFromTrack(trk)
#     ssDataDict[id] = DataFrame(tDict).T
    ssDataDict[trkNo] = tDict
    trkNo += 1
# 
# store = HDFStore(hdfFile)
# for id in ssDataDict.keys():
#     store[id] = ssDataDict[id]
        
    
        