"""
This module defines the base class for accessing netCDF files and using them
with Stride Search.
"""
from netCDF4 import Dataset
from abc import ABCMeta, abstractmethod
from Event import print_copyright
from datetime import date, datetime, timedelta
from IdentCriteria import MaxCriterion, MinCriterion
from numpy import array, amax, amin

class SearchData(object):
    """
    Abstract base class for accessing netCDF data for use with Stride Search.
    This base class defines the interfaces for linking sectors to a particular grid
    and loading sector data from the data file.
    
    Subclass implementations may differ depending on the grid.
    """
    __metaclass__ = ABCMeta
    
    def __init__(self, filename):
        self.fname = filename
        self.dfile = Dataset(filename, "r")

    @abstractmethod
    def initDimensions(self):
        """ Virtual function. Subclasses must implement this function to initialize the netCDF dimensions of a data set.
        """
        pass
        
    @abstractmethod
    def getGridDescription(self):
        """ Virtual function.  Subclasses must implement this function to return a complete description of the data set's grid.
        """
        pass
        
    @abstractmethod
    def getSectorWorkingDataForCriterion(self, crit, sector):
        """ Virtual function.  Subclasses must implement this function to return NumPy arrays containing the relevant
        data for a particular sector and criterion.
        """
        pass
    
    def initTime(self):
        """ Initializes the time dimension from a source file.  
        Creates datetime instances for each time step.  
        
        Need to update for other variable names, e.g., "time_whole" for Aeras
        """
        self.time = self.dfile.variables['time'][:]
        self.datetimes = [datetime(1,10,1,0) + timedelta(days = int(t)) for t in self.time]
        self.nTimesteps = len(self.time)
    
    def updateSourceFile(self, fname):
        """ Updates the source filename and netCDF Dataset.  
        
        Assumption: all files in a data set use the same grid, so no need to 
        update all dimensions.
        """
        self.fname = fname
        self.dfile = Dataset(fname, "r")
    
    def loadFileDataForCriteriaAtTimestep(self, criteria, time_index):
        """ Gets actual data values associated with a sector and from the variables associated with criteria at
        the time step at index = time_index.
        """
        varnames = [var for var in self.dfile.variables]
        self.vardata = {}
        for crit in criteria:
            for var in crit.varnames:
                if var not in varnames:
                    print 'WARNING: variable ', crit.varname, ' not found in data file.'
                else:
                    self.vardata[var] = self.dfile.variables[var][time_index][:][:]
    
    def infoString(self):
        """Returns basic info about the state of an instance of this class.
        """
        str1 = '\tfile start date = ' + str(self.datetimes[0]) + "\n"
        str1 += '\tfile end date = ' + str(self.datetimes[-1]) + "\n"
        return str1

    def printInfo(self):
        """Prints basic instance info to console.
        """
        print self.infoString()
               
    def __repr__(self):
        return "<%s: filename = %s>" \
        %(self.__class__.__name__, self.fname)    

class LatLonSearchData(SearchData):
    """
    """
    def __init__(self, filename):
        SearchData.__init__(self, filename)
    
    def initDimensions(self):
        """
        Finds the dimensions of a uniform lat-lon data set.
        
        Assumptions: Day 0 of data is October 1, Year 1.
            Time units are days since Day 0.
        """
        self.dims = self.dfile.dimensions
        self.lats = self.dfile.variables['lat'][:]
        self.lons = self.dfile.variables['lon'][:]
        self.nLat = len(self.lats)
        self.nLon = len(self.lons)
        
     
    def getGridDescription(self):
        """ Returns the grid description of a uniform latitude-longitude grid.
        
        In this case, the grid consists of 2 integers: the number of latitude points that
        span :math:`-\\pi/2` to :math:`\\pi/2`, 
        and the number of longitude points that span 0 to :math:`2 * pi`.
        """
        return [self.nLat, self.nLon]
     
    def getSectorWorkingDataForCriterion(self, crit, sector):
        """ Load the data required by criterion into the workspace.
        """
        workspace = {}
        for name in crit.varnames:
            varwork = []
            for k in range(len(sector.dataPointIndices)):
                dp = sector.dataPointIndices[k]
                varwork.append(self.vardata[name][dp[0]][dp[1]])
            workspace[name] = array(varwork)
        return workspace
        
                                  
class LatLonSearchDataWithOcean(LatLonSearchData):
    """ For use with applications that require both atmospheric and ocean data (e.g., polar lows).
    
    
    Assumption: Ocean data are on same grid as atm data
    Ocean data are cyclic with period = 1 year, data given in monthly time steps
    Ocean Day 0 = January 1, Year 0
    """
    def __init__(self, atmfile, ocnfile):
        LatLonSearchData.__init__(self, atmfile)    
        self.ocnFname = ocnfile
        self.ocndata = Dataset(ocnfile, "r")
    
    def initTime(self, year_offset = 0):
        LatLonSearchData.initTime(self)
        self.ocnTime = self.ocndata.variables["time"][:]
        self.ocnDates = [date(1 + year_offset, 1, 1) + timedelta(days = t) for t in self.ocnTime]
    
    def loadFileDataForCriteriaAtTimestep(self, atmCrit, ocnCrit, time_index):
        LatLonSearchData.loadFileDataForCriteriaAtTimestep(self, atmCrit, time_index)
        self.ocnVarData = {}
        ocnVars = [var for var in self.ocnData.variables]
        for crit in ocnCrit:
            if crit.varname not in ocnVars:
                print 'WARNING: variable ', ocnCrit.varname, ' not found in ocean data file.'
            else:
                month_int = self.ocnDates[time_index].month
                self.ocnVarData[crit.varname] = self.ocnData.variables[ocnCrit.varname][month_int][:][:]
    
    def getSectorWorkingDataForCriterion2(self, crit, neighborhood, latInds, lonInds):
        work1 = []
        work2 = []
        for k in range(len(neighborhood)):
            if neighborhood[k]:
                work1.append(self.vardata[crit.varname][latInds[k]][lonInds[k]])
                work2.append(self.vardata[crit.varname2][latInds[k]][lonInds[k]])
        return array(work1), array(work2)
    
    def __repr__(self):
        s = LatLonSearchData.__repr__(self)
        return s.split(">")[0] + ", ocnfile = %s>"%(self.ocnFname)
    
    def infoString(self):
        str1 = "atm file = " + self.fname + "\n"
        str1 += LatLonSearchData.infoString(self) + "\n"
        str1 += "ocn file = " + self.ocnFname + "\n"
        str1 += '\tocn file start date = ' + str(self.ocnDates[0]) + "\n"
        str1 += '\tocn file end date = ' + str(self.ocnDates[-1]) + "\n"
        return str1
    
    def printInfo(self):
        print self.infoString()
    
if __name__ == "__main__":
    print_copyright()
    testCount = 0
    passCount = 0
    
    # UNIT TEST 1: Open data file, create stride search data instance
    testfile = "/Users/pabosle/Desktop/dataTemp/f1850c5_ne240_rel06.cam.h2.0002-01-29-00000.nc"
    lldata = LatLonSearchData(testfile)
    print lldata
    testCount += 1
    passCount += 1
    # UNIT TEST 2: Initialize dimensions from file
    lldata.initTime()
    lldata.initDimensions()
    lldata.printInfo()
    testCount += 1
    passCount += 1
    # UNIT TEST 3: Load workspace data for 2 criteria
    c = [MaxCriterion("VOR850", 1.0E-4), MinCriterion('PSL', 900.0)]
    lldata.loadFileDataForCriteriaAtTimestep(c, 4)
    print "max vorticity found = ", amax(lldata.vardata["VOR850"])
    print "min pressure found = ", amin(lldata.vardata["PSL"])
    testCount += 1
    passCount += 1
    
    # UNIT TEST 4: Open data file, atm and ocean files, create data instance
    ocnFile = "/Users/pabosle/Desktop/dataTemp/ocean/sst_HadOIBl_bc_1x1_clim_pi_c101029-513x1024.nc"
    llocn = LatLonSearchDataWithOcean(testfile, ocnFile)
    llocn.initTime(0)
    llocn.initDimensions()
    print llocn
    llocn.printInfo()
    testCount += 1
    passCount += 1

    if passCount == testCount:
        print "SUCCESS: %d of %d tests passed."%(passCount, testCount)
    else:
        print "FAILURE: %d of %d tests passed."%(passCount, testCount)
            
                            
