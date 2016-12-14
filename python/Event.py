"""
This module defines the Stride Search algorithm's fundamental data types that
may be extended for specific applications.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
from datetime import datetime
from math import cos, sin, acos, radians

def print_copyright():
    print """Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, 
there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. 
Export of this program may require a license from the United States Government.\n"""

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
        self.related = []
        self.duplicate = False
          
    def __repr__(self):
        return "Event({0}, {1}, {2}, {3}, {4})".format(repr(self.desc), repr(self.latLon),
             repr(self.datetime), repr(self.dataIndex), repr(self.vals))
     
    def printData(self):
        print "Event data:"
        for att in ['desc', 'latLon', 'datetime', 'dataIndex', 'vals']:
    		print '\t', att, ':', getattr(self, att)
    	if len(self.related) > 0:
    	    for ev in self.related:
    	        print '\trelated:'
    	        for att in ['desc', 'latLon', 'datetime', 'dataIndex', 'vals']:
    	            print '\t\t', att, ':', getattr(ev, att)
    
    def __lt__(self, other):
        return self.datetime < other.datetime
    
    def __gt__(self, other):
        return self.datetime > other.datetime
    
    def __eq__(self, other):
        return self.desc == other.desc and self.datetime == other.datetime and self.dataIndex == other.dataIndex
    
    def dist(self, other):
        return sphereDistance(self.latLon[0], self.latLon[1], other.latLon[0], other.latLon[1])
    
    def isRelated(self, other, radius):
        return self.dist(other) <= radius
    
    def addRelatedEvent(self, other):
        self.related.append(other)

def consolidateEventList(evList, radius):
    newList = []
    # step 1: mark literal duplicates
    eqCount = 0
    for i in range(len(evList)):
        ev = evList[i]
        for j in range(i+1,len(evList)):
            other = evList[j]
            if ev == other:
                other.duplicate = True
                eqCount += 1

    # step 2: mark related entries
    for i in range(len(evList)):
        ev = evList[i]
        for j in range(i+1,len(evList)):
            other = evList[j]
            if not other.duplicate:
                if other.isRelated(ev, radius):
                    ev.addRelatedEvent(other)
                other.duplicate = True
    # step 3: remove duplicates and relateds, return        
    for ev in evList:
        if not ev.duplicate:
            newList.append(ev)
    return newList
                                    
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
    

