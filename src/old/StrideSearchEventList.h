#ifndef _STRIDE_SEARCH_EVENT_LIST_
#define _STRIDE_SEARCH_EVENT_LIST_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchEvent.h"
#include "StrideSearchIDCriterionBase.h"
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include <memory>

namespace StrideSearch {
/// Class for performing operations on a collections of Event instances.
/**
    Consolidates operations like removing duplicate Event instances or finding related Event instances into one class.
    May also be used for track building.
*/
class EventList {
    public:
        /// Default constructor.  Creates an empty list.
        EventList();
        /// Creates an EventList from a vector of Event pointers.
        EventList(const std::vector<std::shared_ptr<Event>>& eventVec);
        /// Creates an EventList by flattening a vector of Event pointer vectors.
        EventList(const std::vector<std::vector<std::shared_ptr<Event>>>& eventVecs);
    
        /// Appends one EventList to another.
        void extend(const EventList& other);
        
        /// Returns the number of Events in *this.
        inline index_type size() const {return events.size();}
    
        /// Returns the minimum separation distance between pairs of Events in *this.
        scalar_type minSeparationDistance() const;
        /// Returns the maximum separation distance between pairs of Events in *this.
        scalar_type maxSeparationDistance() const;
        
        /// Outputs either basic or all information about *this to a string.
        std::string infoString(const int tablevel = 0, const bool printAll = false) const;
        
        /// Removes duplicate and redundant entries from an EventList
        /**
            Duplicates are literal duplicates -- identical Event records.
            Redundant records are Events of the same type and same time, close to each other in space.  
            Only the most intense of the redundant entries will be kept.
        */
        void removeDuplicates(const scalar_type distThreshold);
        
        /// Separates an EventList into multiple EventLists by their DateTime values.
        /**
            The output is a std::map whose keys are DateTime instances and whose values are EventLists whose contained
            Events all exist at the DateTime of their key.
        */
        std::map<DateTime, std::vector<Event>> separateByDate() const;
        
        /// Consolidates related events under one Event listing.
        /** 
            Related Events are events that have different types, but are near each other in space at the same time.
            Assumption: EventList::removeDuplicates has already finished.
        */
        void consolidateRelatedEvents(const scalar_type distThreshold);
        
        /// Removes Events that are either not related to or not near another event of the required type
        void requireCollocation(const IDCriterion* crit1, const IDCriterion* crit2, const scalar_type distThreshold);
        
        /// Access a specific Event contained by *this.
        inline std::shared_ptr<Event> getEvent(const index_type i) const {return events[i];}
    
        void writeASCIIFormatTSTORMS(std::ostream& os) const;
    protected:
        /// Contained Event pointers.
        std::vector<std::shared_ptr<Event>> events;
};


}
#endif
