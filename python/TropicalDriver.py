"""
Stride Tropical Cyclone Search driver.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
from glob import glob
from os import chdir, remove
from datetime import timedelta

from SectorList import SectorListLatLon
from Data import LatLonSearchData
from Event import Event, EventList, print_copyright
from IdentCriteria import MaxSignedCriterion, MinCriterion, CollocationCriterion, VariationExcessCriterion, TimeCriteria
from Track import TrackList, Track
from pandas import DataFrame, Series, HDFStore

print_copyright()
#
#   USER: DEFINE DESIRED OUTPUT
#
verbose = True
write_to_HDF = True
hdfFile = 'tropicalTracks_westpac_OctoberYear2.h5'
save_space_output = True
write_space_output = True
spaceFile = 'strideSearch_spatialResults.h5'

#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"
#ncFiles = ["f1850c5_ne240_rel06.cam.h2.0002-06-28-00000.nc", "f1850c5_ne240_rel06.cam.h2.0002-07-28-00000.nc", 
#            "f1850c5_ne240_rel06.cam.h2.0002-08-27-00000.nc", "f1850c5_ne240_rel06.cam.h2.0002-09-26-00000.nc"]
if write_to_HDF:
    try: 
        remove(hdfFile)
    except:
        pass
if save_space_output and write_space_output:
    try:
        remove(spaceFile)
    except:
        pass
 
ncFiles = ["f1850c5_ne240_rel06.cam.h2.0002-09-26-00000.nc"]  
#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = -40.0
northBnd = 40.0
westBnd = 100.0
eastBnd = 180.0
radius = 450.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
vorMax_to_PSLMin_dist = 450.0
tempMax_to_PSLMin_dist = 225.0

crit1 = MaxSignedCriterion('VOR850', 8.5e-4)
crit2 = MinCriterion('PSL', 99000.0)
crit3 = VariationExcessCriterion('T500', 2.0)
spatialCriteria = [crit1, crit2, crit3]
if verbose:
    print 'identification spatialCriteria set:'
    for crit in spatialCriteria:
        crit.printInfo()
    print ' '

ccrit1 = CollocationCriterion(crit1.returnEventType(), crit2.returnEventType(), vorMax_to_PSLMin_dist)
ccrit2 = CollocationCriterion(crit1.returnEventType(), crit3.returnEventType(), tempMax_to_PSLMin_dist)
collocCriteria = [ccrit1, ccrit2]
if verbose:
    print 'collocation criteria set:'
    for crit in collocCriteria:
        crit.printInfo()
    print ' '

minDuration = 24.0 # hours
maxSpeed = 15.0 # m/s
timeCrit = TimeCriteria(minDuration, maxSpeed)
if verbose:
    print 'time criteria set:'
    timeCrit.printInfo()
    print ' '



### PROGRAM START ###
chdir(dataPath)
#
#   Build search sectors
#
sl = SectorListLatLon(southBnd, northBnd, westBnd, eastBnd, radius)
if verbose:
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
tsHours = timestep_size.seconds / 3600.0
if verbose:
    print "timestep size:", tsHours, 'hours'
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
print "STRIDE SEARCH STEP 1: Spatial search"
L = []
spaceDetections = {}
nTotalTimesteps = 0
nFiles = len(ncFiles)
for fi, dfile in enumerate(ncFiles):
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
    for time_ind in range(ssdata.nTimesteps): #range(20):
        evList = EventList([]) # must be re-initialized with an empty list!
        ssdata.loadFileDataForCriteriaAtTimestep(spatialCriteria, time_ind)
        dt = ssdata.datetimes[time_ind]
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        if verbose:
            print '\tfile %d of %d: processing timestep %d of %d'%(fi+1, nFiles, time_ind+1, ssdata.nTimesteps)
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
            print '\t...found ', len(evList), ' events matching spatial criteria.'
        L.append(evList)
        if save_space_output:
            lls = []
            for ev in evList:
                lls.append(ev.latLon)
            spaceDetections[ssdata.datetimes[time_ind]] = Series(lls)
        
if verbose:
    print "Spatial search complete: ", len(L), " time steps searched."  

if save_space_output and write_space_output:
    llS = DataFrame(spaceDetections)   
    spcStore = HDFStore(spaceFile)
    spcStore['latlon'] = llS
          

#
#   Stride Search Part II: Temporal correlation
#   NOTE: This section is serial
#
print "STRIDE SEARCH STEP 2: Temporal correlation"
#   temporal algorithm starts with empty list of storm tracks, T
# 
TL = TrackList(timeCrit, tsHours)
TL.buildTracksFromSpatialResults(L)
dfList = TL.getDataFrameList()
if verbose:
    print "TrackList built, dataframe returned, len = ", len(dfList)
    
if write_to_HDF:
    store = HDFStore(hdfFile)
    for i, df in enumerate(dfList):
        label = "track_" + str(i)
        store[label] = df
        