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
from Event import Event, print_copyright
from IdentCriteria import MaxCriterion, MinCriterion

print_copyright()
#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

#
#   USER: DEFINE SEARCH REGION, EVENT RADIUS
#
southBnd = -90.0
northBnd = 90.0
westBnd = 0.0
eastBnd = 360.0
radius = 1000.0

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
criteria = [MaxCriterion('VORBOT', 1.0e-4), MinCriterion('PSL', 990.0)]


#
#   Get list of all netCDF files to search
#
chdir(dataPath)
ncFiles = glob("*.nc")

#
#   Build search sectors
#
sl = SectorListLatLon(southBnd, northBnd, westBnd, eastBnd, radius)
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
    print '...reading file: %s'%(dfile)
    ssdata.updateSourceFile(dfile)
    ssdata.initTime()
    #
    #   Loop over time steps (NOTE: this loop is embarassingly parallel)
    #
    for time_ind in range(ssdata.nTimesteps):
        ssdata.loadFileDataForCriteriaAtTimestep(criteria, time_ind)
        #
        #   Loop over sectors (NOTE: this loop is embarassingly parallel)
        #
        print '\t...processing timestep %s of %s'%(time_ind, ssdata.nTimesteps)
        for k in range(sl.nSectors):
            sec = sl.findSectorInData(k, ssdata)
            for crit in criteria:
                wspc = ssdata.getSectorWorkingDataForCriterion(crit, sec)

                
                
                    

if __name__ == "__main__":
    print sl
    print ssdata
    
    