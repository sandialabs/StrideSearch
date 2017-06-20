#ifndef _STRIDE_SEARCH_EVENT_H_
#define _STRIDE_SEARCH_EVENT_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include <string>
#include <iostream>
#include <vector>
#include <memory>

namespace StrideSearch {

/// A record of an important (as deemed by IDCriterion methods) event in the data.
/**
    An Event is a record of a the data and location associated with an instance of a set of identification criteria
    (a collection of IDCriterion subclasses) evaluating as True in a Sector.
    
    For example, if an IDCriterion is defined to identify vorticity maxima greater than a threshold, when it encounters
    such a value in the data, it creates an Event with a description like "VortMax", the data value, location in 
    physical space (lat-lon coords), location in time as a DateTime instance, location in data space ll_index_type currently,
    the filename that contains the Event, the time index in that file that corresponds to the Event's time, and the Event
    type enum.
    
*/
class Event {
    public:
        friend class EventList;
        friend class CollocationCriterion;
        friend class Track;
        /// Types of events
        enum IntensityComparison {LESS_THAN, GREATER_THAN};
        
        /// Constructors
        Event();
        Event(const std::string dsc, const scalar_type value, const ll_coord_type ll, const DateTime dt, 
            const vec_indices_type& locIndex, const std::string fname, const index_type tind, const IntensityComparison tp);
        /// Destructor
        virtual ~Event(){};
        
        /// Description of Event returned in a string.
        std::string infoString(int tabLevel = 0) const;
        
        /// Add a related event to this event.
        /**
            Related Events are different events that correspond to the same data feature.
            For example, a vorticity maxima may be associated with a related pressure minima.
            
            @todo Require related Events to have different types
        */
        void addRelated(std::shared_ptr<Event> relEv); 
          
        /// Evaluates True if other is an Event of lower intensity (as determined by the EventType) than this.
        bool lowerIntensity(const Event& other) const; 
        
        /// Evalates True if other is an exact duplicate of this.
        /**
            Duplicates may occur when overlapping Sector s detect the same event.
        */
        inline bool isDuplicate(const Event& other) const {
            return desc == other.desc && datetime == other.datetime && dataIndex == other.dataIndex;
        }
        
        /// Evaluates true if 2 events are near each other, where near is defined as a distance threshold.
        bool isNear(const Event& other, const double distThreshold) const;
        
        /// Evaluates true if two Events of the same type with different values and/or locations are near each other.
        /**
            Redundant Events are events of the same type that are closely spaced; for example, two sectors may find the same
            Event based on their own local data, but the data may be slightly different between the two sectors.
        */
        bool isRedundant(const Event& other, const double distThreshold) const;
        
        scalar_type minRelatedDistance() const;
        
        scalar_type maxRelatedDistance() const;
                
    protected:
        std::string desc;
        scalar_type val;
        ll_coord_type latLon;
        DateTime datetime;
        std::vector<index_type> dataIndex;
        std::string filename;
        index_type time_index;
        std::vector<std::shared_ptr<Event>> relatedEvents;
        bool isReferenced;
        IntensityComparison compare;
};
inline bool operator == (const Event& left, const Event& right) {return left.isDuplicate(right);}
inline bool operator < (const Event& left, const Event& right) {return left.lowerIntensity(right); }
inline bool operator > (const Event& left, const Event& right) {return right < left; }

}
#endif
