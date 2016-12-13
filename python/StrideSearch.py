"""
This module defines the Stride Search algorithm's fundamental data types that
may be extended for specific applications.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
import math
        
class SectorList(object):
    """
    The SectorList class lists the centers of each Stride Search circular 
    search sector.
    The search region boundaries are given as a rectangle in latitude-longitude
    coordinates (degrees).
    A representative search radius (in kilometers) that corresponds to an 
    Event's size defines the search sectors' size.  
    
    This class must be extended to link with data sets on various grids.
    """
    def __init__(self, southBnd, northBnd, westBnd, eastBnd, radius_km):
        self.southBnd = southBnd
        self.northBnd = northBnd
        self.westBnd = westBnd
        self.eastBnd = eastBnd
        self.r = radius_km
        self.buildSectorList()
    
    def __repr__(self):
        return "SectorList({0}, {1}, {2}, {3}, {4})".format(repr(self.southBnd), repr(self.northBnd), 
            repr(self.westBnd), repr(self.eastBnd), repr(self.r))

    def buildSectorList(self):
        """
        Given a search region's boundaries, this function computes the
        search sector centers for a given Event radius (kilometers).
        """
        sector_arc_length = self.r / earthRadius_km
        nStrips = int(math.floor((math.radians(self.northBnd) - math.radians(self.southBnd)) / sector_arc_length))
        stripWidth = (self.northBnd - self.southBnd) / nStrips
        sectorLats = [self.southBnd + i * stripWidth for i in range(nStrips+1)]
        lonStrides = [min([self.r / (earthRadius_km * math.cos(math.radians(sectorLats[i]))), 2.0 * math.pi]) 
            for i in range(nStrips+1)]
        lonStrideDegrees = [math.degrees(stride) for stride in lonStrides]     
        sectorCenters = []
        self.numCenters = 0
        self.stripLen = []
        for i in range(nStrips+1):
            nLons = int(math.ceil((self.eastBnd - self.westBnd) / lonStrideDegrees[i]))
            for j in range(nLons):
                cnt = (sectorLats[i], self.westBnd + j * lonStrideDegrees[i])
                sectorCenters.append(cnt)   
        self.centers = sectorCenters
        self.latStrideReal = stripWidth
        self.lonStrideReals = lonStrideDegrees
                   
    def setupSectorsForData(self, gridDescription):
        """
        Placeholder function. In subclasses, this function will compute any 
        necessary auxilliary data required by the topology of a data set's grid.
        """
        raise NotImplementedError('setupSectorsForData must be defined by a subclass!')
    
    def linkSectorToData(self, sector_index):
        """
        Placeholder function. In subclasses, this function will define the data
        points from a data set that correspond to a specific search sector
        (identified by its index)
        """
        raise NotImplementedError('linkSectorToData must be defined by a subclass!')
    
class SectorList_LatLonGrid(SectorList):
    """
    An implementation of Stride Search SectorList for use with uniform latitude-longitude data sets.
    """
    def __init__(self, southBnd, northBnd, westBnd, eastBnd, radius_km, nLat, nLon):
        super(SectorList_LatLonGrid, self).__init__(southBnd, northBnd, westBnd, eastBnd, radius_km)
        self.nLat = nLat
        self.nLon = nLon
        self.setupSectorsForData(None)
    
    def __repr__(self):
        return "SectorList_LatLonGrid({0}, {1}, {2}, {3}, {4}, {5}, {6})".format(repr(self.southBnd), repr(self.northBnd), 
            repr(self.eastBnd), repr(self.westBnd), repr(self.r), repr(self.nLat), repr(self.nLon))

    def setupSectorsForData(self, null_arg):  
        """
        Defines sectors in data space for uniform lat-lon grids.
        """
        dataRes = 360.0 / self.nLon
        self.latStrideInt = int(math.floor(math.radians(self.latStrideReal) * self.nLon / (2.0 * math.pi)) + 1)
        self.lonStrideInts = [int(math.floor(math.radians(lonStr) * self.nLon / (2.0 * math.pi)) + 1) 
            for lonStr in self.lonStrideReals]
        self.latMinIndex = int(math.floor((self.southBnd + 90.0) / dataRes))
        self.latMaxIndex = int(math.floor((self.northBnd + 90.0) / dataRes))
        self.lonMinIndex = int(math.floor(self.westBnd / dataRes))
        self.lonMaxIndex = int(math.floor(self.eastBnd / dataRes))
       
        
    def linkSectorToData(self, sector_index):
        
        

            
if __name__ == "__main__":
    print_copyright()
    ev = Event(45.0, 0.0, 2016, 12, 8, 13)
    print "ev.latlon() = ", ev.latlon()  
    print ev
    #sc_list = SectorList(0.0, 80.0, 180.0, 360.0, 1000.0) 
    #print sc_list
    #print "Sector centers : ", sc_list.centers
    sc_ll = SectorList_LatLonGrid(-90.0, 80.0, 180.0, 360.0, 1000.0, 91, 180) 
    print sc_ll
    print sc_ll.lonStrideInts
    #print "Sector centers : ", sc_ll.centers

             
    
        
        
