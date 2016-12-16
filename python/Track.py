"""
"""
from Event import Event, sphereDistance, print_copyright 
from datetime import timedelta
from IdentCriteria import TimeCriteria
from numpy import shape

mpsToKph = 3.6

class Track(object):
    """
    A Track is an ordered list (ascending in time) of related Events. 
    """    
    def __init__(self, evList = []):
        self.events = evList
    
    def __repr__(self):
        pass
        
    def __len__(self):
        return len(self.events)   
    
    def addEventToTrack(self, ev):
        self.events.append(ev)
    
    def isEmpty(self):
        return len(self.events) == 0
    
    def printData(self):
        print "track data: len = ", len(self)
        for ev in self.events:
            ev.printData()
        print "-----------"
    
    def startDate(self):
        return self.events[0].datetime
    
    def endDate(self):
        return self.events[-1].datetime
        
    def duration(self):
        return self.endDate() - self.startDate()

class TrackList(object):
    """
    TrackList container that also constructs each Track.
    
    minDuration = minimum duration of Event in hours (hours)
    maxEventSpeed = maximum speed of Event in meters per second (m/s)
    delT_hours = time step size in hours of the data set (hours)  
    """
    def __init__(self, tc, tsHours):
        self.minDuration = tc.minDuration
        self.maxSpeed = tc.maxSpeed
        self.timestepsize = tsHours
        self.maxKmPerTimeStep = self.maxSpeed * mpsToKph * tsHours 
        self.tracks = []
    
    def __len__(self):
        return len(self.tracks)
    
    def printData(self):
        maxLen = len(self.tracks[0])
        minLen = len(self.tracks[0])
        for track in self.tracks:
            if len(track) > maxLen:
                maxLen = len(track)            
            if len(track) < minLen:
                minLin = len(track)
                
        print "TrackList summary: list length = ", len(self), ": max track len = ", \
            maxLen, ", min track len = ", minLen
        for track in self.tracks:
            lls = {}
            for ev in track.events:
                lls[ev.datetime] = ev.latLon
            print lls
    
    def buildTracksFromSpatialResults(self, listOfEventLists):
        """
        This method assumes that the EventLists in listOfEventLists are listed in 
        ascending time-step order.
        """
        alreadyUsed = [ [False for ev in el] for el in listOfEventLists]
        trackCounter = 0
        for dtInd, evList in enumerate(listOfEventLists):
            for evInd, ev in enumerate(evList):
                #
                #   start from the oldest unused event in the list of event lists
                #
                if not alreadyUsed[dtInd][evInd]:
                    possibleTrack = Track([])
                    possibleTrack.addEventToTrack(listOfEventLists[dtInd][evInd])
#                     print "possible track ", trackCounter + 1, " start"
                    currentEv = listOfEventLists[dtInd][evInd]
                    tInd = dtInd
                    
                    keepGoing = True                    
                    while keepGoing:
                        #
                        #   search each subsequent timestep for this event's successors
                        #
                        if tInd + 1 == len(listOfEventLists):
                            # end of data
                            keepGoing = False
                            break
                        # search next time step for candidate successors
                        for cInd, cev in enumerate(listOfEventLists[tInd+1]):
                            candidates = {}
                            if not alreadyUsed[tInd+1][cInd]:
                                if currentEv.isNear(cev, self.maxKmPerTimeStep):
                                    # candidate sucessors = nearby events at next time step
                                    candidates[cInd] = cev
                        if len(candidates) == 0:
                            # end of track
                            keepGoing = False
                        elif len(candidates) == 1:
                            # one successor found -- it's the next leg of the track
                            possibleTrack.addEventToTrack(candidates[candidates.keys()[0]])
                            currentEv = candidates[candidates.keys()[0]]
                            alreadyUsed[tInd+1][candidates.keys()[0]] = True
                        else:
                            # multiple successors found; choose closest one
                            d = currentEv.dist(candidates[candidates.keys()[0]])
                            nextInd = 0
                            for ccInd, ccev in enumerate(candidates):
                                d2 = currentEv.dist(ccev)
                                if d2 < d:
                                    d = d2
                                    nextInd = ccInd
                            possibleTrack.addEventToTrack(candidates[candidates.keys()[nextInd]])
                            currentEv = candidates[candidates.keys()[nextInd]]
                            alreadyUsed[tInd+1][candidates.keys()[nextInd]] = True
                        tInd += 1
                    trackCounter += 1
                    self.tracks.append(possibleTrack)
        marked = [False for trk in self.tracks]
        for ind, trk in enumerate(self.tracks):
            if len(trk) < 2:
                marked[ind] = True
        nt = []
        for ind, trk in enumerate(self.tracks):
            if not marked[ind]:
                nt.append(trk)
        self.tracks = nt
                                        
    
if __name__ == '__main__':
    print_copyright()    
    