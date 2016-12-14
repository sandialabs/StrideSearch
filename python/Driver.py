"""
Stride Search driver.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
from glob import glob
from os import chdir
from SectorList import SectorListLatLon
from Data import LatLonSearchData
from Event import Event, print_copyright, consolidateEventList
from IdentCriteria import MaxCriterion, MinCriterion

print_copyright()
#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = 50.0
northBnd = 60.0
westBnd = 150.0
eastBnd = 160.0
radius = 250.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
criteria = [MaxCriterion('VORBOT', 3.0e-4), MinCriterion('PSL', 99000.0)]
print 'identification criteria set:'
for crit in criteria:
    crit.printData()

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
#
#   Setup input data readers
#
ssdata = LatLonSearchData(ncFiles[0])
ssdata.initTime()
ssdata.initDimensions()
#
#   Give sectors info about the grid
#
gridDesc = [ssdata.nLat, ssdata.nLon]
sl.setupSectorsForData(gridDesc)
#
# search starts with empty event list
#
L = []
#
#   Loop over files (NOTE: this loop is embarassingly parallel)
#
for dfile in ncFiles:
    print 'reading file: %s ...'%(dfile)
    ssdata.updateSourceFile(dfile)
    ssdata.initTime()
    ssdata.printData()
    #
    #   Loop over time steps (NOTE: this loop is embarassingly parallel)
    #
    for time_ind in range(2):
        evList = []
        ssdata.loadFileDataForCriteriaAtTimestep(criteria, time_ind)
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        print '\t...processing timestep %s of %s'%(time_ind, ssdata.nTimesteps)
        for k in range(sl.nSectors):
            sec = sl.findSectorInData(k, ssdata)
            for crit in criteria:
                wspc = ssdata.getSectorWorkingDataForCriterion(crit, sec)
                if crit.evaluate(sec, wspc):
                    evList.append(crit.returnEvent(sec, wspc, ssdata.datetimes[time_ind]))
        el = consolidateEventList(evList, radius)
        L = L + el
                
print "found " + str(len(L)) + " events matching criteria."     
           
if len(L) <= 20:
    for ev in L:
        ev.printData()

if __name__ == "__main__":
    print sl
    print ssdata
    
    