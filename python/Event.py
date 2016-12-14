"""
This module defines the Stride Search algorithm's fundamental data types that
may be extended for specific applications.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
from datetime import datetime

def print_copyright():
    print """Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, 
there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. 
Export of this program may require a license from the United States Government.\n\n"""

#def earthRadius_km():
#    """Earth mean sea level radius in kilometers"""
#    return 6371.220
  
class Event(object):
    """
    An Event is a storm or other relevant event to a user of this software.
    At a minimum, an Event has a date and location that correspond to a data index.
    """
    def __init__(self, desc, lat, lon, year, month, day, hour, dataIndex = None):
        self.desc = desc
        self.lat = lat
        self.lon = lon
        self.datetime = datetime(year, month, day, hour);
        self.dataIndex = dataIndex
        
    def latlon(self):
        """
        Get the (lat, lon) tuple of an Event.
        """
        return (self.lat, self.lon)
    
    def __repr__(self):
        return "Event({0}, {1}, {2}, {3}, {4}, {5}, {6}, {7})".format(repr(self.desc), repr(self.lat), repr(self.lon), 
            repr(self.datetime.year), repr(self.datetime.month), repr(self.datetime.day), 
            repr(self.datetime.hour), repr(self.dataIndex))

            
                                    
if __name__ == "__main__":
    print_copyright()
    ev = Event('unitTest', 45.0, 0.0, 2016, 12, 8, 13)
    print "ev.latlon() = ", ev.latlon()  
    print ev

