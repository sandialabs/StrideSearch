"""
"""
from Event import Event, print_copyright , emptyEvent
from datetime import timedelta
from IdentCriteria import TimeCriteria
from pandas import DataFrame, Series
from numpy import nan, shape, array

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
    
    def infoString(self, verbose = False):
        str1 = "track data: len = " + str(len(self)) + "\n"
        if verbose:
            for ev in self.events:
                str1 += ev.infoString() + "\n-----------\n"
        return str1
    
    def getLatLons(self):
        lldict = {}
        for ev in self.events:
            lldict[ev.datetime] = ev.latLon
        return lldict
        
    def printInfo(self, verbose = False):
        istr =  self.infoString()
        if verbose:
            print istr
        else:
            print istr[0]

    def startDate(self):
        return self.events[0].datetime
    
    def endDate(self):
        return self.events[-1].datetime
    
    def getDates(self):
        dateList = []
        for ev in self.events:
            if ev.datetime not in dateList:
                dateList.append(ev.datetime)
        return dateList
            
    def duration(self):
        return self.endDate() - self.startDate()

    def getEventTypes(self):
        tlist = []
        for ev in self.events:
            if ev.desc not in tlist:
                tlist.append(ev.desc)
            for relEv in ev.related:
                if relEv.desc not in tlist:
                    tlist.append(relEv.desc)
        return tlist
        
    
    def getDataFrame(self):
        dts = [ev.datetime for ev in self.events]
        lls = [ev.latLon for ev in self.events]
        typelist = self.getEventTypes()
        trackData = { tt : Series(nan, index = dts) for tt in typelist}
        trackData["lat-lon"] = Series(lls, index=dts)
        for evInd, ev in enumerate(self.events):
            unpck = ev.unpackRelated()
            for unpackedEv in unpck:
                key = unpackedEv.vals.keys()[0]
                trackData[unpackedEv.desc][dts[evInd]] = unpackedEv.vals[key]
        return DataFrame(trackData)    
            
            
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
        self.minTrackLength = int(self.minDuration // self.timestepsize)
        self.tracks = []
    
    def __len__(self):
        return len(self.tracks)
    
    def __getitem__(self, ind):
        return self.tracks[ind]    
    
    def addTrack(self, trk):
        self.tracks.append(trk)
    
    def infoString(self, verbose = False):
        maxLen = len(self.tracks[0])
        minLen = maxLen
        for track in self.tracks:
            if len(track) > maxLen:
                maxLen = len(track)            
            if len(track) < minLen:
                minLen = len(track)
                
        str1 =  "TrackList summary: list length = " + str(len(self)) + ": max track len = " + \
            str(maxLen) + ", min track len = " + str(minLen) + "\n"
        str1 += "\tmin duration = " + str(self.minDuration) + ", max speed = " + str(self.maxSpeed) + ", dt = " + str(self.timestepsize) + ", max dist per timestep = " + str(self.maxKmPerTimeStep) + "\n"
        if verbose:
            for track in self.tracks:
                lls = {}
                for ev in track.events:
                    lls[ev.datetime] = ev.latLon
                str1 += str(lls) + "\n.................\n"
        return str1
    
    def printInfo(self):
        print self.infoString()
    
    def getDataFrameList(self):
        dfl = []
        for trk in self.tracks:
            dfl.append(trk.getDataFrame())
    
    def buildTracksFromSpatialResults(self, eventListsByDate):
        """
        Input: eventListsByDate: A dictionary whose entries are key = datetime, value = EventList whose Events all have that datetime
        Output: tracks
        """
        alreadyUsed = {dt : [] for dt in eventListsByDate}
        for dt in eventListsByDate:
            for ev in eventListsByDate[dt]:
                alreadyUsed[dt].append(False)
        tsInterval = timedelta(hours = self.timestepsize)        
        #print alreadyUsed
        trackCounter = 0
        for dtg in eventListsByDate:
            evList = eventListsByDate[dtg]
            for i, ev in enumerate(evList):
                if not alreadyUsed[dtg][i]:
                    possibleTrack = Track([])
                    possibleTrack.addEventToTrack(ev)
                    alreadyUsed[dtg][i] = True
                    trackCounter += 1
                    
                    currentEv = ev
                    currentDtg = dtg
                    keepGoing = True
                    
                    nextEv = emptyEvent()
                    #debugString = 'track %d, %s, fileInfo: %s'%(trackCounter, repr(dtg), ev.dataIndex)
                    while keepGoing:
                        if currentDtg == max(eventListsByDate.keys()): 
                            # end of data
                            keepGoing = False
                            break
                        nextDtg = currentDtg + tsInterval
                        nextEv = currentEv
                        if nextDtg in eventListsByDate:
                            candidates = {}
                            for j, cand in enumerate(eventListsByDate[nextDtg]):
                                if not alreadyUsed[nextDtg][j]:
                                    if currentEv.isNear(cand, self.maxKmPerTimeStep):
                                        candidates[j] = cand
                            if len(candidates) == 0:
                                # no sucessors found, end of track
                                keepGoing = False
                            elif len(candidates) == 1:
                                # unique sucessor found
                                nextKey = candidates.keys()[0]
                                nextEv = candidates[nextKey]
                                possibleTrack.addEventToTrack(nextEv)
                                alreadyUsed[nextDtg][nextKey] = True
                            else:
                                # multiple candidates found, choose closest one as successor
                                nextKey = candidates.keys()[0]
                                nextEv = candidates[nextKey]                                
                                d = currentEv.dist(nextEv)
                                for j in candidates:
                                    cand = candidates[j]
                                    d2 = currentEv.dist(cand)
                                    if d2 < d:
                                        d = d2
                                        nextEv = cand
                                        nextKey = candidates.keys()[j]
                                possibleTrack.addEventToTrack(nextEv)
                                alreadyUsed[nextDtg][nextKey] = True
                        else:
                            keepGoing = False
                        currentDtg = nextDtg
                        currentEv = nextEv
                    #debugString += ", len = %d"%(len(possibleTrack))
                    #print debugString
                    if len(possibleTrack) >= self.minTrackLength:
                        #print "adding track ", trackCounter, ", len = ", len(possibleTrack)
                        self.addTrack(possibleTrack)
                        #del possibleTrack
                                
                                
                            
                            
        
    # def buildTracksFromSpatialResults(self, listOfEventLists):
#         """
#         This method assumes that the EventLists in listOfEventLists are listed in 
#         ascending time-step order.
#         """
#         alreadyUsed = [ [False for ev in el] for el in listOfEventLists]
#         trackCounter = 0
#         for dtInd, evList in enumerate(listOfEventLists):
#             for evInd, ev in enumerate(evList):
#                 #
#                 #   start from the oldest unused event in the list of event lists
#                 #
#                 if not alreadyUsed[dtInd][evInd]:
#                     possibleTrack = Track([])
#                     possibleTrack.addEventToTrack(listOfEventLists[dtInd][evInd])
# #                     print "possible track ", trackCounter + 1, " start"
#                     currentEv = listOfEventLists[dtInd][evInd]
#                     tInd = dtInd
#                     
#                     keepGoing = True                    
#                     while keepGoing:
#                         #
#                         #   search each subsequent timestep for this event's successors
#                         #
#                         if tInd + 1 == len(listOfEventLists):
#                             # end of data
#                             keepGoing = False
#                             break
#                         # search next time step for candidate successors
#                         if currentEv.datetime + timedelta(hours = self.timestepsize) == listOfEventLists[tInd+1][0].datetime:
#                             for cInd, cev in enumerate(listOfEventLists[tInd+1]):
#                                 candidates = {}
#                                 if not alreadyUsed[tInd+1][cInd]:
#                                     if currentEv.isNear(cev, self.maxKmPerTimeStep):
#                                         # candidate sucessors = nearby events at next time step
#                                         candidates[cInd] = cev
#                             if len(candidates) == 0:
#                                 # end of track
#                                 keepGoing = False
#                             elif len(candidates) == 1:
#                                 # one successor found -- it's the next leg of the track
#                                 possibleTrack.addEventToTrack(candidates[candidates.keys()[0]])
#                                 currentEv = candidates[candidates.keys()[0]]
# #                                 try :
#                                 alreadyUsed[tInd+1][candidates.keys()[0]] = True
# #                                 except IndexError:
# #                                     print "TrackBuild ERROR : index ", (tInd + 1, candidates.keys()[0])
# #                                     print "shape(listOfEventLists) = ", shape(listOfEventLists)
# #                                     print "shape(alreadyUsed) = ", shape(alreadyUsed)
# #                                     print "other indices: dtInd = ", dtInd, ", evInd = ", evInd
#                             else:
#                                 # multiple successors found; choose closest one
#                                 d = currentEv.dist(candidates[candidates.keys()[0]])
#                                 nextInd = 0
#                                 for ccInd, ccev in enumerate(candidates):
#                                     d2 = currentEv.dist(ccev)
#                                     if d2 < d:
#                                         d = d2
#                                         nextInd = ccInd
#                                 possibleTrack.addEventToTrack(candidates[candidates.keys()[nextInd]])
#                                 currentEv = candidates[candidates.keys()[nextInd]]
#                                 alreadyUsed[tInd+1][candidates.keys()[nextInd]] = True
#                         tInd += 1
#                     trackCounter += 1
#                     self.tracks.append(possibleTrack)
#         marked = [False for trk in self.tracks]
#         for ind, trk in enumerate(self.tracks):
#             if len(trk) <= 2:
#                 marked[ind] = True
#         nt = []
#         for ind, trk in enumerate(self.tracks):
#             if not marked[ind]:
#                 nt.append(trk)
#         self.tracks = nt
                                        
    
if __name__ == '__main__':
    print_copyright()    
    