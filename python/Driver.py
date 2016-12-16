"""
Stride Search driver.

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
from IdentCriteria import MaxCriterion, MinCriterion, TimeCriteria, CollocationCriterion
from Track import TrackList, Track

print_copyright()
#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = 40.0
northBnd = 80.0
westBnd = 110.0
eastBnd = 240.0
radius = 500.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
crit1 = MaxCriterion('VORBOT', 3.0e-4)
crit2 = MinCriterion('PSL', 99000.0)
spcCrit = [crit1, crit2]
requireCollocation = True
colloc = [CollocationCriterion(crit1.returnEventType(), crit2.returnEventType(), radius)]

minDuration = 24.0 # hours
maxSpeed = 15.0 # m/s
timeCrit = TimeCriteria(minDuration, maxSpeed)
print 'identification spcCrit set:'
for crit in spcCrit:
    crit.printData()
print ' '

#
#   USER: DEFINE DESIRED OUTPUT
#
verbose = False
outputSpatialSearch = True
spSearchFile = dataPath + "ssSpaceOutput.txt"
if outputSpatialSearch:
	try:
	    remove(spSearchFile)
	except:
		pass


### PROGRAM START ###
#
#   Get list of all netCDF files to search
#
chdir(dataPath)
ncFiles = glob("*29-00000.nc")

#
#   Build search sectors
#
sl = SectorListLatLon(southBnd, northBnd, westBnd, eastBnd, radius)
print 'sector list built:'
sl.printData()
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
nTotalTimesteps = 0
for dfile in ncFiles:
    print 'reading file: %s ...'%(dfile)
    ssdata.updateSourceFile(dfile)
    ssdata.initTime()
    ssdata.printData()
    nTotalTimesteps += ssdata.nTimesteps
    #
    #   Loop over time steps (NOTE: this loop is embarassingly parallel)
    #
    for time_ind in range(20): #range(20):
        evList = EventList([]) # must be re-initialized with an empty list!
        ssdata.loadFileDataForCriteriaAtTimestep(spcCrit, time_ind)
        dt = ssdata.datetimes[time_ind]
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        if verbose:
            print '\t...processing timestep %s of %s'%(time_ind, ssdata.nTimesteps)
        for k in range(sl.nSectors):
            sec = sl.findSectorInData(k, ssdata)
            for crit in spcCrit:
                wspc = ssdata.getSectorWorkingDataForCriterion(crit, sec)
                if crit.evaluate(sec, wspc):
                    ev = crit.returnEvent(sec, wspc, dt)
                    evList.addEvent(ev)
        evList.removeDuplicates(radius)
        evList.consolidateRelated(radius)
        if requireCollocation:
	        evList.requireCollocation(colloc)
        if verbose:
            print '\t...found ', len(evList), ' events matching spatial criteria.'
        L.append(evList)
         
print "Spatial search complete: ", len(L), " time steps searched."
# for i, el in enumerate(L):
#     print "len(L[", i, "]) = ", len(el)
# for el in L:
#     el.printData()
         
           

        
#
#   Stride Search Part II: Temporal correlation
#   NOTE: This section is serial
#
print "STRIDE SEARCH STEP 2: Temporal correlation"
#   temporal algorithm starts with empty list of storm tracks, T
# 
TL = TrackList(timeCrit, tsHours)
TL.buildTracksFromSpatialResults(L)
TL.printData()



     

if __name__ == "__main__":
    pass
    
    