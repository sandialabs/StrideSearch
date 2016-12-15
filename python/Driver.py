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
from IdentCriteria import MaxCriterion, MinCriterion, TimeCriteria
from Track import TrackList, Track

print_copyright()
#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = 45.0
northBnd = 80.0
westBnd = 160.0
eastBnd = 210.0
radius = 400.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
spcCrit = [MaxCriterion('VORBOT', 4.5e-4), MinCriterion('PSL', 96000.0)]
minDuration = 24.0 # hours
maxSpeed = 15.0 # m/s
print 'identification spcCrit set:'
for crit in spcCrit:
    crit.printData()
print ' '

#
#   USER: DEFINE DESIRED OUTPUT
#
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
ncFiles = glob("*.nc")

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
L = EventList([])
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
    for time_ind in range(2):
        evList = EventList([])
        ssdata.loadFileDataForCriteriaAtTimestep(spcCrit, time_ind)
#         print "data for datetime ", ssdata.datetimes[time_ind], " loaded."
        dt = ssdata.datetimes[time_ind]
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        print '\t...processing timestep %s of %s'%(time_ind, ssdata.nTimesteps)
        for k in range(sl.nSectors):
            sec = sl.findSectorInData(k, ssdata)
            for crit in spcCrit:
                wspc = ssdata.getSectorWorkingDataForCriterion(crit, sec)
                if crit.evaluate(sec, wspc):
#                     print "dt in = ", dt
                    ev = crit.returnEvent(sec, wspc, dt)
#                     ev.printData()
                    evList.addEvent(ev)
#                     print "dt out = ", ev.datetime
#         print "***** time_ind = ", time_ind, " *****"
#         evList.printData()
#         print "==================="
        evList.removeDuplicates(radius)
#         evList.printData()
#         print "********************"
        evList.consolidateRelated(radius)
        L.extend(evList)
         
print "Spatial search complete:"       
print "\tfound " + str(len(L)) + " events matching spcCrit."     
           
if outputSpatialSearch:
    if len(L) <= 20:
        for ev in L:
            ev.printData()
    else:
        pass
        
#
#   Stride Search Part II: Temporal correlation
#   NOTE: This section is serial
#
print "STRIDE SEARCH STEP 2: Temporal correlation"
#   temporal algorithm starts with empty list of storm tracks, T
# 
# T = TrackList(minDuration, maxSpeed, tsHours)
# for i in range(nTotalTimesteps):
#   starttime = datastarttime + timedelta(hours = i * tsHours)
#   startIndices = L.getIndicesAtTime(starttime)
#   for j in range(len(L)):
#       if not L[j].duplicate:
#           T.buildNewTrack(j, L)
            
            

if __name__ == "__main__":
    pass
    
    