"""@package StrideSearch
This module defines the Event and EventList classes; it is the base module in the StrideSearch package.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.

@author: Peter A. Bosler
"""
from datetime import datetime
from math import cos, sin, atan2, radians, pi, sqrt

def print_copyright():
    """Prints Stride Search copyright information"""
    print """Stride Search. Copyright 2016 Sandia Corporation. Under the terms of contract DE-AC04-94AL85000, 
there is a non-exclusive license for use of this work by or on behalf of the U.S. Government. 
Export of this program may require a license from the United States Government.\n"""

def print_citation():
    """Prints the bibtex citation for Stride Search"""
    print "@article{StrideSearch,"
    print "\tauthor = {P. A. Bosler and E. L. Roesler and Mark A. Taylor and Miranda R. Mundt},"
    print "\ttitle = {Stride Search: a general algorithm for storm detection in high-resolution climate data},"
    print "\tjournal = {Geoscientific Model Development},"
    print "\tvolume = {9},"
    print "\tyear = {2016},"
    print "\tpages = {1383--1398},"
    print "\tdoi = {10.5194/gmd-9-1383-2016}}"

earthRadius_km = 6371.220

def sphereDistance(lat1, lon1, lat2, lon2):
    """
    Returns the great circle distance between two points on an earth-sized sphere.
    Assumes input is given in degrees.
    """
    __ZERO_TOL = 2.0e-9
    if abs(lat2 - lat1) < __ZERO_TOL and abs(lon2 - lon1) < __ZERO_TOL:
            return 0.0
    else:
        xa = cos(radians(lat1)) * cos(radians(lon1))
        ya = cos(radians(lat1)) * sin(radians(lon1))
        za = sin(radians(lat1))
        xb = cos(radians(lat2)) * cos(radians(lon2))
        yb = cos(radians(lat2)) * sin(radians(lon2))
        zb = sin(radians(lat2))
        
        cp1 = ya * zb - yb * za
        cp2 = xb * za - xa * zb
        cp3 = xa * yb - xb * ya
            
        cpnorm = sqrt(cp1 * cp1 + cp2 * cp2 + cp3 * cp3)
        
        dp = xa * xb + ya * yb + za * zb
        
        arclen = atan2(cpnorm, dp)
        return earthRadius_km * arclen

class EventList(object):
    def __init__(self, evList = []):
        self.events = evList
    
    def addEvent(self, ev):
        self.events.append(ev)
        
    def extend(self, other):
        self.events.extend(other.events)
        
    def __getitem__(self, ind):
        return self.events[ind]
    
    def __len__(self):
        return len(self.events)
    
    def eventTypes(self):
        tt = []
        for ev in self.events:
            if ev.desc not in tt:
                tt.append(ev.desc)
            for relEv in ev.related:
                if relEv.desc not in tt:
                    tt.append(relEv.desc)
        return tt
    
    def maxSeparationDistance(self):
        d = 0.0
        for i, ev in enumerate(self.events):
            for j in range(i+1, len(self.events)):
                d2 = ev.dist(self.events[j])
                if d2 > d:
                    d = d2
        return d
    
    def minSeparationDistance(self):
        d = 2.0 * pi * earthRadius_km
        for i, ev in enumerate(self.events):
            for j in range(i+1, len(self.events)):
                d2 = ev.dist(self.events[j])
                if d2 < d:
                    d = d2
        return d
    
    def infoString(self):
        str1 = "Event list data:\n"
        for ev in self.events:
            str1 += ev.infoString()
        return str1 
    
    def printInfo(self):
        print self.infoString()
    
    def removeDuplicates(self, radius):
        """
        Removes duplicate items from the list. If an Event lies in the overlap
        region between sectors, each sector may detect the same Event independently.
        
        Independent detections may have slightly different locations/values; we keep only the most intense one.
        """
        duplicates = [False for ev in self.events]
        # step 1: mark duplicates
        eqCount = 0
        for i, evA in enumerate(self.events):
            if not duplicates[i]:
                for j in range(i+1, len(self.events)):
                    if not duplicates[j]:
                        evB = self.events[j]
                        if evA.isDuplicate(evB):
                            duplicates[j] = True
                            eqCount += 1

        # step 2: mark lower intensity detections of same event
        relCount = 0
        for tt in self.eventTypes():
            for i, evA in enumerate(self.events):
                if evA.desc == tt and not duplicates[i]:
                    for j in range(i+1,len(self.events)):
                        if not duplicates[j]:
                            evB = self.events[j]                  
                            if evB.desc == tt and evA.datetime == evB.datetime:
                                if evA.isNear(evB, radius):
                                    relCount += 1
                                    if evA > evB:
                                        duplicates[j] = True
                                    else:
                                        duplicates[i] = True
#         print "eqCount = ", eqCount, ", relCount = ", relCount, ", sum = ", \
#             eqCount + relCount, ", count = ", duplicates.count(True)
        sizein = len(self.events)                                        
        newevents = []
        for i, ev in enumerate(self.events):
            if not duplicates[i]:
                newevents.append(ev)
        sizeout = len(newevents)
#         print "sizein = ", sizein, ", sizeout = ", sizeout, ", diff = ", sizein - sizeout
        self.events = newevents
     
    def consolidateRelated(self, radius):
        """
        Consolidates events of different types (e.g., vorticity max or pressure min) that correspond to the same physical event into one event + related events sublist.
        This method assumes that self.removeDuplicates() has already been called to remove similar events of the same type.
        """
        alreadyUsed = [False for ev in self.events]
        for i, evA in enumerate(self.events):
            if not alreadyUsed[i]:
                for j in range(i+1, len(self.events)):
                    evB = self.events[j]
                    if not alreadyUsed[j]:
                        if evA.isNear(evB, radius) and evA.datetime == evB.datetime:
                            evA.addRelatedEvent(evB)
                            alreadyUsed[j] = True
        newevents = []
        for i, ev in enumerate(self.events):
            if not alreadyUsed[i]:
                newevents.append(ev)
        self.events = newevents     
      
    def requireCollocation(self, collocCriteria):
        newevents = []
        for ev in self.events:
            collCount = 0
            for crit in collocCriteria:
                if crit.evaluate(ev):
                    collCount += 1
            if collCount == len(collocCriteria):
                newevents.append(ev)
        self.events = newevents
                
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
        self.otherData = {}
          
    def __repr__(self):
        return "Event({0}, {1}, {2}, {3}, {4})".format(repr(self.desc), repr(self.latLon),
             repr(self.datetime), repr(self.dataIndex), repr(self.vals))
     
    def infoString(self):
        str1 = "Event record:\n"
        for att in ['desc', 'latLon', 'datetime', 'dataIndex', 'vals']:
            str1 += '\t' + att + ':' + str(getattr(self, att)) + "\n"
        if len(self.related) > 0:
            str1 += '\trelated events:\n'
            str1 += "\tmax separtion = %g km\n"%(self.separationDistance()) 
        for ev in self.related:
            for att in ['desc', 'latLon', 'datetime', 'dataIndex', 'vals']:
                str1 += '\t\t' + att + ':' + str(getattr(ev, att)) + "\n"
            str1 += '\t\t----------'
        str1 += '----------\n'
        return str1
    
    def unpackRelated(self):
        elist = [Event(self.desc, self.latLon, self.datetime, self.dataIndex, self.vals)]
        for relEv in self.related:
            elist.append(relEv)
        return EventList(elist)
    
    def appendOtherData(self, key, val):
        self.otherData[key] = val    
    
    def printInfo(self):
        print self.infoString()
        
    def sameType(self, other):
        return self.desc == other.desc
    
    def __lt__(self, other):
        """Event less_than (<) implies a lower-intensity event"""
        if self.sameType(other):
            if "max" in self.vals.keys()[0]:
                return self.vals.keys()[0] < other.vals.keys()[0]
            elif "min" in self.vals.keys()[0]:
                return self.vals.keys()[0] > other.vals.keys()[0]
        else:
            raise TypeError("Event comparison ERROR: type mismatch")
            
    def __gt__(self, other):
        """Event greater_than (>) implies a higher-intensity event"""
        if self.sameType(other):
            if "max" in self.vals.keys()[0]:
                return self.vals.keys()[0] > other.vals.keys()[0]
            elif "min" in  self.vals.keys()[0]:
                return self.vals.keys()[0] < other.vals.keys()[0]
        else:
            raise TypeError("Event comparison ERROR: type mismatch")
    
    def separationDistance(self):
        d = 0.0
        for ev in self.related:
            d2 = self.dist(ev)
            if d2 > d:
                d = d2
        return d
    
    def isDuplicate(self, other):
        """Equivalent events have the exact same datetime and data point values."""
        return self.desc == other.desc and self.datetime == other.datetime and self.dataIndex == other.dataIndex
    
    def dist(self, other):
        """Returns the great-circle distance between two Events."""
        return sphereDistance(self.latLon[0], self.latLon[1], other.latLon[0], other.latLon[1])
    
    def isNear(self, other, radius):
        """Two Events are "near" each other if they are separated by a distance smaller than the supplied radius."""
        return self.dist(other) < radius
    
    def isRelated(self, other, radius):
        return self.datetime == other.datetime and self.isNear(other, radius)
    
    def addRelatedEvent(self, other):
        """Related Events are events of different types (e.g., pressure min and vorticity max) that correspond to the same meteorological feature."""
        self.related.append(other)

                                    
if __name__ == "__main__":
    print_copyright()
    
    print "BibTex citation:"
    print_citation()
    
    print ' '
    print "*RUNNING UNIT TESTS: Event class*"
    #
    #   meaningless data
    #
    dt = datetime.now()
    dtin = datetime(dt.year, dt.month, dt.day, dt.hour)
    desc = 'unitTest'
    ll1 = (45.0, 0.0)
    ll2 = (45.2, 0.2)
    inds1 = (1,1)
    inds2 = (1,2)
    val1 = {'min' : 990.0}
    val2 = {'min' : 992.0}
    ll3 = (44.8, 0.1)
    inds3 = (0,1)
    val3 = {'max' : 2.0e-4}
    ll4 = (50.2, 90.0)
    inds4 = (45, 80)
    val4 = {'max', 1.9e-4}
    
    print "ev1 :"   
    evPSL1 = Event('PSL min', ll1, dtin, inds1, val1)
    print evPSL1
    print evPSL1.infoString()
    
    evPSL2 = Event('PSL min', ll2, dtin, inds2, val2)
#     print "ev2 : "
#     evPSL2.printInfo()
    
    evVor1 = Event('Vort. max', ll3, dtin, inds3, val3)
#     print "ev3 : "    
#     evVor1.printInfo()
    
    
    evVor2 = Event('Vort. max', ll4, dtin, inds4, val4)
#     print "ev4 :"
#     evVor2.printInfo()
    
    print "ev1 == ev2 (False): ", evPSL1 == evPSL2
    print "ev1 > ev2 (True): ", evPSL1 > evPSL2
    print "ev1 < ev2 (False): ", evPSL1 < evPSL2
    print "ev1.isNear(ev2) (True): ", evPSL1.isNear(evPSL2, 500.0)
    print "ev1.isRelated(ev2) (True): ", evPSL1.isRelated(evPSL2, 500.0)
    print "ev4.isRelated(ev1) (False): ", evVor2.isRelated(evPSL1, 500.0)
    
    print "*tests complete*\n"
    
    
    print "*RUNNING UNIT TESTS: EventList class*"
    evList1 = EventList([evPSL1, evPSL2, evVor1])
    evList1.addEvent(evVor2)
    print "length (4): ", len(evList1)
#     evList1.printInfo()
    evList2 = evList1
    evList1.extend(evList2)
    print "length (8): ", len(evList1)
    evList1.removeDuplicates(500.0)
    print "length (3): ", len(evList1)
    evList1.consolidateRelated(500.0)
    print "length (2): ", len(evList1)
    evList1.printInfo()
    print "access event[0]: ", evList1[0]
    print "types: ", evList1.eventTypes()

