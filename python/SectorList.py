"""
Stride Search Sectors module.
"""
from Event import Event, print_copyright
from abc import ABCMeta, abstractmethod
from math import floor, ceil, radians, degrees, cos, sin, acos, pi
from numpy import zeros, shape


earthRadius_km = 6371.220

def sphereDistance(lat1, lon1, lat2, lon2):
    """
    Returns the great circle distance between two points on an earth-sized sphere.
    Assumes input is given in degrees.
    """
    __ZERO_TOL = 2.0e-14
    if abs(lat2 - lat1) < __ZERO_TOL and abs(lon2 - lon1) < __ZERO_TOL:
        return 0.0
    else:
        arclen = acos(sin(radians(lat1)) * sin(radians(lat2)) +
            cos(radians(lat1)) * cos(radians(lat2)) * cos(radians(lon2) - radians(lon1)))
        return earthRadius_km * arclen

class Sector(object):
    """
    Sectors are the units of work in Stride Search.
    Each Sector is defined by its center (lat, lon), its radius (km), its latStride and lonStride, as well
    as the data points/indices that are contained within the sector's geographic boundaries.
    
    Sectors may be constructed by the SectorList::findSectorInData function.
    """
    __metaclass__ = ABCMeta
    def __init__(self, centerLat, centerLon, radius, latStride, lonStride, dataPoints, dataPointIndices):
        self.centerLat = centerLat
        self.centerLon = centerLon
        self.radius = radius
        self.latStride = latStride
        self.lonStride = lonStride
        self.dataPoints = dataPoints
        self.dataPointIndices = dataPointIndices

    def __repr__(self):
        return """<%s: centerLat = %s, centerLon = %s, radius = %s, latStride = %s, lonStride = %s
            datapoints = %s, datapointIndices = %s>"""%(self.__class__.__name__, 
            self.centerLat, self.centerLon, self.radius, self. latStride, self.lonStride, 
            repr(self.dataPoints), repr(self.dataPointIndices))

class SectorList(object):
    """
    SectorList contain the sector centers defined by the Stride Search algorithm.
    
    Search region boundaries are defined in degrees.
    Latitudes in [-90.0, 90.0], Longitudes in [0.0, 360.0].
    radius_km is the Event radius for a particular search application.
    """
    __metaclass__ = ABCMeta
    
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
        nn = int(floor( (radians(self.northBnd) - radians(self.southBnd)) / sector_arc_length))
        stripWidth = (self.northBnd - self.southBnd) / nn
        centerLats = [self.southBnd + i * stripWidth for i in range(nn+1)]
        lonStrides = [min([self.radius / (earthRadius_km * cos(radians(centerLats[i]))), 2.0 * pi]) 
            for i in range(nn+1)]
        lonStrideDegrees = [degrees(stride) for stride in lonStrides]
        self.centerLats = []
        self.centerLons = []
        self.stripID = []
        for i in range(nn+1):
            nLonsPerStrip = int(ceil((self.eastBnd - self.westBnd) / lonStrideDegrees[i]))
            for j in range(nLonsPerStrip):
                self.centerLats.append(centerLats[i])
                self.centerLons.append(self.westBnd + j * lonStrideDegrees[i])
                self.stripID.append(i)
        self.nStrips = nn + 1             
        self.latStride = stripWidth
        self.lonStrides = lonStrides     
        self.lonStrideDegrees = lonStrideDegrees
        self.nSectors = len(self.centerLats)   

    @abstractmethod
    def setupSectorsForData(self, gridDesc):
        pass

    @abstractmethod
    def findSectorInData(self, sector_index, ssdata):
        pass
               
    def __repr__(self):
        return "<%s: southBoundary = %s, northBoundary= %s, westBoundary = %s, eastBoundary = %s, radius = %s>" \
        %(self.__class__.__name__, self.southBnd, self.northBnd, self.westBnd, self.eastBnd, self.radius)

class SectorListLatLon(SectorList):
    def setupSectorsForData(self, gridDesc):
        self.nLat = gridDesc[0]
        self.nLon = gridDesc[1]
        print "nLat = ", self.nLat, " nLon = ", self.nLon
        self.dLambda = 2.0 * pi / self.nLon
        self.dataRes = 360.0 / self.nLon
        self.latMinIndex = int(floor((self.southBnd + 90.0) / self.dataRes))
        self.latMaxIndex = int(floor((self.northBnd + 90.0) / self.dataRes))
        self.lonMinIndex = int(floor(self.westBnd / self.dataRes))
        self.lonMaxIndex = int(floor(self.eastBnd / self.dataRes))
        self.latStrideInteger = int(floor(self.latStride / self.dataRes))
        self.lonStrideIntegers = [int(floor(stride / self.dataRes)) + 1 for stride in self.lonStrideDegrees]
        
    def findSectorInData(self, sector_index, ssdata):
        centerLatIndex = int(floor((self.centerLats[sector_index] + 90.0) / self.dataRes))
        centerLonIndex = int(floor(self.centerLons[sector_index] / self.dataRes))
        lonStrideInt = self.lonStrideIntegers[self.stripID[sector_index]]
        sectorLatIndices = []
        sectorLatValues = []
        sectorLonIndices = []
        sectorLonValues = []
        for i in range( max(self.latMinIndex, centerLatIndex - self.latStrideInteger), 
            min(self.latMaxIndex, centerLatIndex + self.latStrideInteger)):
            if centerLonIndex - lonStrideInt < 0:
                #
                #   sector crosses longitude = 0.0
                #
                for j in range(centerLonIndex + lonStrideInt - 1):
                    sectorLatIndices.append(i)
                    sectorLatValues.append(ssdata.lats[i])
                    sectorLonIndices.append(j)
                    sectorLonValues.append(ssdata.lons[j])
                for j in range(self.nLon -1 + centerLonIndex - lonStrideInt, self.nLon-1):
                    sectorLatIndices.append(i)
                    sectorLatValues.append(ssdata.lats[i])
                    sectorLonIndices.append(j)
                    sectorLonValues.append(ssdata.lons[j])
            elif centerLonIndex + lonStrideInt > self.nLon - 1:
                #
                #   sector crosses longitude = 360.0
                #
                for j in range(centerLonIndex - lonStrideInt, self.nLon - 1):
                    sectorLatIndices.append(i)
                    sectorLatValues.append(ssdata.lats[i])
                    sectorLonIndices.append(j)
                    sectorLonValues.append(ssdata.lons[j])
                for j in range(lonStrideInt + self.nLon - 1 - centerLonIndex):
                    sectorLatIndices.append(i)
                    sectorLatValues.append(ssdata.lats[i])
                    sectorLonIndices.append(j)
                    sectorLonValues.append(ssdata.lons[j])
            else:
                for j in range(centerLonIndex - lonStrideInt, centerLonIndex + lonStrideInt):
                    sectorLatIndices.append(i)
                    sectorLatValues.append(ssdata.lats[i])
                    sectorLonIndices.append(j)
                    sectorLonValues.append(ssdata.lons[j])
     
        neighborhoodLats = []
        neighborhoodLons = []
        neighborhoodLatInds = []
        neighborhoodLonInds = []     
        for k in range(len(sectorLatIndices)):
            if sphereDistance(sectorLatValues[k], sectorLonValues[k],
                    self.centerLats[sector_index], self.centerLons[sector_index]) <= self.radius:
                neighborhoodLats.append(sectorLatValues[k])
                neighborhoodLatInds.append(sectorLatIndices[k])
                neighborhoodLons.append(sectorLonValues[k])
                neighborhoodLonInds.append(sectorLonIndices[k])
                     
        return Sector(self.centerLats[sector_index], self.centerLons[sector_index],
            self.radius, self.latStride, self.lonStrideDegrees[self.stripID[sector_index]], 
            zip(neighborhoodLats, neighborhoodLons), zip(neighborhoodLatInds, neighborhoodLonInds))



if __name__ == "__main__":
    print_copyright()
    scl = SectorListLatLon(0.0, 80.0, 180.0, 360.0, 1000.0)
    lldesc = [91,180]
    scl.setupSectorsForData(lldesc)
    print(scl)
