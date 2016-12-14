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
Export of this program may require a license from the United States Government.\n"""

#def earthRadius_km():
#    """Earth mean sea level radius in kilometers"""
#    return 6371.220
  
class Event(object):
    """
    An Event is a storm or other relevant event to a user of this software.
    At a minimum, an Event has a date and location that correspond to a data index,
    and a set of key-value pairs (vals) describing the Event.
    """
    def __init__(self, desc, latLon, dtime, dataIndex = None, vals = None):
        self.desc = desc
        self.latLon = latLon
        self.datetime = dtime;
        self.dataIndex = dataIndex
        self.vals = vals
          
    def __repr__(self):
        return "Event({0}, {1}, {2}, {3}, {4})".format(repr(self.desc), repr(self.latLon),
             repr(self.datetime), repr(self.dataIndex), repr(self.vals))
     
    def printData(self):
        for att in ['desc', 'latLon', 'datetime', 'dataIndex', 'vals']:
    		print '\t', att, ':', getattr(self, att)
                                    
if __name__ == "__main__":
    print_copyright()
    dt = datetime.now()
    dtin = datetime(dt.year, dt.month, dt.day, dt.hour)
    desc = 'unitTest'
    ll = (45.0, 0.0)
    inds = (1,1)
    vals = {'testval' : 100.0}
    ev = Event(desc, ll, dtin, inds, vals)
    print 'Event created:'
    print ev
    print 'Event data:'
    ev.printData()
    

