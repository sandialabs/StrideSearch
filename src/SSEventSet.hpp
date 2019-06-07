#ifndef _SS_EVENT_SET_HPP_
#define _SS_EVENT_SET_HPP_

#include "SSEvent.hpp"
#include "SSDateTime.hpp"
#include <vector>
#include <memory>
#include <map>
#include <string>

namespace StrideSearch {

template <typename DL> class Track;

/// Class for performing operations on a collections of Event instances.
/**
    Consolidates operations like removing duplicate Event instances or finding related Event instances into one class.
    May also be used for track building.
*/
template <typename DataLayout=UnstructuredLayout>
class EventSet {
    public:
        typedef std::shared_ptr<Event<DataLayout>> event_ptr_type;
        typedef std::shared_ptr<const Event<DataLayout>> const_event_ptr_type;
    
        template <typename DL> friend class Track;
    
        /// Default constructor.  Creates an empty set.
        EventSet() : events() {}
        
        /// Creates an EventSet from a vector of Event pointers.
        EventSet(const std::vector<event_ptr_type>& events_) : events(events_) {}
        
        /// Creates an EventSet by flattening a vector of Event pointer vectors.
        EventSet(const std::vector<std::vector<event_ptr_type>>& events_);
        
        /// Appends one EventSet to another
        void extend(const EventSet<DataLayout>& other);
        
        /// Append a single event to a set
        void append(const event_ptr_type ep) {events.push_back(ep);}
        
        /// Returns the number of Events in *this
        inline Index size() const {return events.size();}
        
        /// Returns the minimum distance between all Event pairs in *this
        Real minSeparationDistance() const;
        
        /// Returns the maximum distance between all Event pairs in *this
        Real maxSeparationDistance() const;
        
        /// Removes both duplicates and redundant Events from *this
        /**
            Duplicates are literal duplicates -- identical Event records.
            Redundant records are Events of the same type and same time, close to each other in space.  
            Only the most intense of the redundant entries will be kept.
        */
        void removeDuplicates(const Real dist_tol);
        
        /// Outputs either basic or all information about *this to a string.
        std::string infoString(const int tab_lev=0, const bool print_all=false) const;
        
        /// Separates an EventList into multiple EventLists by their DateTime values.
        /**
            The output is a std::map whose keys are DateTime instances and whose values are EventLists whose contained
            Events all exist at the DateTime of their key.
        */
        std::map<DateTime,std::vector<event_ptr_type>> separateByDateTime() const;
        
        /// Consolidates related events under one Event listing.
        /** 
            Related Events are events that have different types, but are near each other in space at the same time.
            @note Assumption: EventSet::removeDuplicates has already finished.
        */
        void consolidateRelated(const Real dist_tol);
        
        /// Removes Events that are either not related to or not near another event of the required type
        /**
            @note: Assumption: EventSet::removeDuplicates and EventSet::consolidateRelated have already finished.
        */
        void requireCollocation(const std::shared_ptr<IDCriterion> crit1, const std::shared_ptr<IDCriterion> crit2,
            const Real dist_tol);
        
        /// Access a specific Event contained by *this.
        inline event_ptr_type getEvent(const Index ind) {return events[ind];}
        
        inline const_event_ptr_type getEventConst(const Index ind) const {return events[ind];}
    
//         void writeData(std::ostream& os);
        
    protected:
        std::vector<event_ptr_type> events;
};

}
#endif
