"""
Stride Search Sectors module.
"""
from Event import print_copyright
from abc import ABCMeta, abstractmethod
from math import floor, ceil, radians, degrees, cos, pi

"""Earth mean sea level radius in kilometers"""
earthRadius_km = 6371.220

#class Sector(object):
#    __metaclass__ = ABCMeta
#    
#    def __init__(self, lat, lon, radius_km):
#        self.lat = lat
#        self.lon = lon
#        self.radius = radius_km
#    def __repr__(self):
#        return "<%s: lat = %s, lon = %s, radius = %s>"%(self.__class__.__name__, self.lat, self.lon, self.radius)
#        
#    @abstractmethod 
#    def registerDataPoints(self):
#        pass
#
#class UnifLatLonSector(Sector):
#    def registerDataPoints(self, latVals, latInds, lonVals, lonInds):
#        self.latVals = latVals
#        self.latInds = latInds
#        self.lonVals = lonVals
#        self.lonInds = lonInds

class SectorList(object):
    """
    SectorList contain the sector centers defined by the Stride Search algorithm.
    
    Search region boundaries are defined in degrees.
    Latitudes in [-90.0, 90.0], Longitudes in [0.0, 360.0].
    radius_km is the Event radius for a particular search application.
    """
    __metaclass = ABCMeta
    
    def __init__(self, southBnd, northBnd, westBnd, eastBnd, radius_km):
        self.southBnd = southBnd
        self.northBnd = northBnd
        self.westBnd = westBnd
        self.eastBnd = eastBnd
        self.radius = radius_km
        self.buildSectorList()
        
    def buildSectorList(self):
        """
        Computes and stores search sector centers that are separated by approximately
        one event radius.
        """
        sector_arc_length = self.radius / earthRadius_km
        nStrips = int(floor( (radians(self.northBnd) - radians(self.southBnd)) / sector_arc_length))
        stripWidth = (self.northBnd - self.southBnd) / nStrips
        centerLats = [self.southBnd + i * stripWidth for i in range(nStrips+1)]
        lonStrides = [min([self.radius / (earthRadius_km * cos(radians(centerLats[i]))), 2.0 * pi]) 
            for i in range(nStrips+1)]
        lonStrideDegrees = [degrees(stride) for stride in lonStrides]
        self.centerLats = []
        self.centerLons = []
        for i in range(nStrips+1):
            nLonsPerStrip = int(ceil((self.eastBnd - self.westBnd) / lonStrideDegrees[i]))
            for j in range(nLonsPerStrip):
                self.centerLats.append(centerLats[i])
                self.centerLons.append(self.westBnd + j * lonStrideDegrees[i])
        self.latStride = stripWidth
        self.lonStrides = lonStrides        

    def __repr__(self):
        return "<%s: southBoundary = %s, northBoundary= %s, westBoundary = %s, eastBoundary = %s, radius = %s>" \
        %(self.__class__.__name__, self.southBnd, self.northBnd, self.westBnd, self.eastBnd, self.radius)

if __name__ == "__main__":
    print_copyright()
    scl = SectorList(0.0, 80.0, 180.0, 360.0, 1000.0) 
    print(scl)