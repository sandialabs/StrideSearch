"""
This module defines the base class for accessing netCDF files and using them
with Stride Search.
"""
from netCDF4 import Dataset
from abc import ABCMeta, abstractmethod
from Event import print_copyright
from datetime import date, datetime, timedelta
from IdentCriteria import MaxCriterion, MinCriterion

class SearchData(object):
    __metaclass__ = ABCMeta
    
    def __init__(self, filename):
        self.fname = filename
        self.dfile = Dataset(filename, "r")

    @abstractmethod
    def initDimensions(self):
        pass
    
    def updateTime(self):
        """
        Need to update for other variable names, e.g., "time_whole" for Aeras
        """
        self.time = self.dfile.variables['time'][:]
        self.nTimesteps = len(self.time)
    
    def updateSourceFile(self, fname):
        """
        Assumption: all files in a data set use the same grid, so no need to 
        update all dimensions.
        """
        self.fname = fname
        self.dfile = Dataset(fname, "r")
        self.updateTime()
    
    def loadFileDataForCriteriaAtTimestep(self, criteria, time_index):
        varnames = [var for var in self.dfile.variables]
        self.vardata = []
        for crit in criteria:
            if crit.varname not in varnames:
                print 'WARNING: variable ', crit.varname, ' not found in data file.'
            else:
                self.vardata.append(self.dfile.variables[crit.varname][time_index][:][:])

    def __repr__(self):
        return "<%s: filename = %s>" \
        %(self.__class__.__name__, self.fname)    

class LatLonSearchData(SearchData):
    """
    """
    def initDimensions(self):
        """
        Finds the dimensions of a uniform lat-lon data set.
        Assumptions: Day 0 of data is October 1, Year 1.
            Time units are days since Day 0.
        """
        self.dims = self.dfile.dimensions
        self.updateTime()
        self.lats = self.dfile.variables['lat'][:]
        self.lons = self.dfile.variables['lon'][:]
        self.nLat = len(self.lats)
        self.nLon = len(self.lons)
        self.datetimes = [datetime(1,10,1,0) + timedelta(days = t) for t in self.time]
        
class LatLonSearchDataWithOcean(LatLonSearchData):
    """
    Assumption: Ocean data are on same grid as atm data
    Ocean data are cyclic with period = 1 year, data given in monthly time steps
    Ocean Day 0 = January 1, Year 0
    """
    def __init__(self, atmfile, ocnfile):
        LatLonSearchData.__init__(self, atmfile)    
        self.ocnFname = ocnfile
        self.ocnData = Dataset(ocnfile, "r")
    
    def updateTime(self, year_offset = 0):
        LatLonSearchData.updateTime(self)
        offset = timedelta(years = year_offset)
        self.ocnTime = self.ocnData.variables["time"][:]
        self.ocnDates = [date(0, 1, 1) + offset + timedelta(days = t) for t in self.ocnTime]
    
    def loadFileDataForCriteriaAtTimestep(self, atmCrit, ocnCrit, time_index):
        LatLonSearchData.loadFileDataForCriteriaAtTimestep(self, atmCrit, time_index)
        self.ocnVarData = []
        ocnVars = [var for var in self.ocnData.variables]
        for crit in ocnCrit:
            if crit.varname not in ocnVars:
                print 'WARNING: variable ', ocnCrit.varname, ' not found in ocean data file.'
            else:
                month_int = self.ocnDates[time_index].month
                self.ocnVarData.append(self.ocnData.variables[ocnCrit.varname][month_int][:][:])
    #TODO: __repr__
    
if __name__ == "__main__":
    print_copyright()
    testfile = "/Users/pabosle/Desktop/dataTemp/f1850c5_ne240_rel06.cam.h2.0002-01-29-00000.nc"
    ocnFile = "/Users/pabosle/Desktop/dataTemp/sst_HadOIBl_bc_1x1_clim_pi_c101029-513x1024.nc"
    lldata = LatLonSearchData(testfile)
    lldata.initDimensions()
    print lldata
    print "data start = ", lldata.datetimes[0]
    print "data end = ", lldata.datetimes[-1]
    c = [MaxCriterion("VOR850", 1.0E-4), MinCriterion('PSL', 900.0)]
    lldata.loadFileDataForCriteriaAtTimestep(c, 4)

            