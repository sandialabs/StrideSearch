import datetime
import math
import numpy as np

earthRadius_km = 6371.220

class Event:
    def __init__(self, lat, lon, year, month, day, hour, dataIndex = None):
        self.lat = lat
        self.lon = lon
        self.datetime = datetime.datetime(year, month, day, hour);
        self.dataIndex = dataIndex
        
    def latlon(self):
        return (self.lat, self.lon)
    
    def __repr__(self):
        return "Event({0}, {1}, {2}, {3}, {4}, {5}, {6})".format(repr(self.lat), repr(self.lon), 
            repr(self.datetime.year), repr(self.datetime.month), repr(self.datetime.day), 
            repr(self.datetime.hour), repr(self.dataIndex))
        
#
#   is this class necessary?  Couldn't it just be a list of latlon tuples?
#
class SectorList(object):
    def __init__(self, southBnd, northBnd, westBnd, eastBnd, radius_km):
        self.southBnd = southBnd
        self.northBnd = northBnd
        self.westBnd = westBnd
        self.eastBnd = eastBnd
        self.r = radius_km
        self.buildSectorList()
    
    def __repr__(self):
        return "SectorList({0}, {1}, {2}, {3}, {4})".format(repr(self.southBnd), repr(self.northBnd), 
            repr(self.eastBnd), repr(self.westBnd), repr(self.r))

    def buildSectorList(self):
        sector_arc_length = self.r / earthRadius_km
        nStrips = int(math.floor((math.radians(self.northBnd) - math.radians(self.southBnd)) / sector_arc_length))
        stripWidth = (self.northBnd - self.southBnd) / nStrips
        self.latStrideReal = stripWidth
        sectorLats = [self.southBnd + i * stripWidth for i in range(nStrips+1)]
        lonStrides = [min([self.r / (earthRadius_km * math.cos(math.radians(sectorLats[i]))), 2.0 * math.pi]) 
            for i in range(nStrips+1)]
        lonStrideDegrees = [math.degrees(stride) for stride in lonStrides]
        self.lonStrideReals = lonStrideDegrees
        sectorCenters = []
        for i in range(nStrips+1):
            nLons = int(math.ceil((self.westBnd - self.eastBnd) / lonStrideDegrees[i]))
            lons_i = [self.eastBnd + j * lonStrideDegrees[i] for j in range(nLons)]
            sectorCenters.append([sectorLats[i],lons_i])
        self.centers = sectorCenters
        
    def setupSectorsForData(self, gridDescription):
        raise NotImplementedError('setupSectorsForData must be defined by a subclass!')
    
    def linkSectorToData(self, sector_index):
        raise NotImplementedError('linkSectorToData must be defined by a subclass!')
    
class SectorList_LatLonGrid(SectorList):
    def __init__(self, southBnd, northBnd, westBnd, eastBnd, radius_km, nLat, nLon):
        super(SectorList_LatLonGrid, self).__init__(southBnd, northBnd, westBnd, eastBnd, radius_km)
        self.nLat = nLat
        self.nLon = nLon
#         self.setupSectorsForData(None)
    
    def __repr__(self):
        return "SectorList({0}, {1}, {2}, {3}, {4}, {5}, {6})".format(repr(self.southBnd), repr(self.northBnd), 
            repr(self.eastBnd), repr(self.westBnd), repr(self.r), repr(self.nLat), repr(self.nLon))

    def setupSectorsForData(self, null_arg):     
        dLambda = 2.0 * math.pi / self.nLon
        dataRes = 360.0 / self.nLon
        latStrideInt = int(math.floor(math.radians(self.latStrideReal) * self.nLon / (2.0 * math.pi)) + 1)
        lonStrideInts = [int(math.floor(math.radians(lonStr) * self.nLon / (2.0 * math.pi)) + 1) 
            for lonStr in self.lonStrideReals]
        
    def linkSectorToData(self, sector_index):
        pass   

            
if __name__ == "__main__":
    ev = Event(45.0, 0.0, 2016, 12, 8, 13)
    print "ev.latlon() = ", ev.latlon()  
    print ev
    sc_list = SectorList(0.0, 80.0, 180.0, 360.0, 1000.0) 
    print sc_list
    print sc_list.centers
    sc_ll = SectorList_LatLonGrid(0.0, 80.0, 180.0, 360.0, 1000.0, 91, 181) 
    print sc_ll

             
    
        
        
