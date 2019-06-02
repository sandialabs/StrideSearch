#ifndef _STRIDE_SEARCH_EVENT_H_
#define _STRIDE_SEARCH_EVENT_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include <string>
#include <set>
#include <iostream>
#include <vector>
#include <memory>

namespace StrideSearch {

// fwd declaration
class IDCriterion;

/// A record of an important (as deemed by IDCriterion methods) event in the data.
/**
    An Event is a record of a the data and location associated with an instance of a set of identification criteria
    (a collection of IDCriterion subclasses) evaluating as True in a Sector.
    
    For example, if an IDCriterion is defined to identify vorticity maxima greater than a threshold, when it encounters
    such a value in the data, it creates an Event with a description like "VortMax", the data value, location in 
    physical space (lat-lon coords), location in time as a DateTime instance, location in data space ll_index_type currently,
    the filename that contains the Event, the time index in that file that corresponds to the Event's time, and the Event
    type enum.
    
    The Event class is one of the fundamental StrideSearch classes (along with Sector and IDCriterion).
*/
class Event {
    public:
        friend class EventList;
        friend class Track;
        /// Types of events
        enum IntensityComparison {LESS_THAN, GREATER_THAN, NO_COMPARE};
        
        /// Default constructor.  Creates an empty event.  Rarely used.
        Event();
        /// Primar constructor.  Defines a complete Event record.
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
            
            To require related Events to have different types, use EventList methods EventList::removeDuplicates and
            EventList::consolidateRelated.
        */
        void addRelated(std::shared_ptr<Event> relEv); 
          
        /// Evaluates True if other is an Event of lower intensity (as determined by the EventType) than this.
        bool lowerIntensity(const Event& other) const; 
        
        /// Evalates True if other is an exact duplicate of this.
        /**
            Duplicates may occur when overlapping Sectors detect the same event.
        */
        inline bool isDuplicate(const Event& other) const {
            return desc == other.desc && datetime == other.datetime && dataIndex == other.dataIndex;
        }
        
        /// Evaluates true if 2 events are near each other, where near is defined as a distance threshold.
        bool isNear(const Event& other, const double distThreshold) const;
        
        inline bool sameDateTime(const Event& other) const {return datetime == other.datetime;}
        
        /// Evaluates true if two Events of the same type with different values and/or locations are near each other.
        /**
            Redundant Events are events of the same type that are closely spaced; for example, two sectors may find the same
            Event based on their own local data, but the data may be slightly different between the two sectors.
        */
        bool isRedundant(const Event& other, const double distThreshold) const;
        
        /// The minimum distance separating Events related to *this.
        scalar_type minRelatedDistance() const;
        
        /// The maximum distance separating Events related to *this.
        scalar_type maxRelatedDistance() const;
        
        /// Geodesic distance betweeen *this and an other Event.
        scalar_type distance(const Event& other) const;
        
        inline const DateTime* getDateTime() const {return &datetime;}
        
        std::string tstormsEntry() const;
        
        /// Returns the latitude-longitude coordinate of *this.
        inline ll_coord_type location() const {return latLon;}
        
        /// True if *this and one of its related events are collocated in space.
        /**
            An output of "true" implies that *this and its related Events have exactly one record associated with 
            IDCriterion crit1 and exactly one record associated with IDCriterion crit2.  
            Furthermore, the locations of these two records are separated by a distance less than or equal to
            the distanceThreshold.
            
            Assumption 1: EventList::consolidateRelated has already finished.
            Assumption 2: *this and its relatedEvents have the same time_index (this is enforced by Event::addRelated()).
        */
        bool isCollocated(const IDCriterion* crit1, const IDCriterion* crit2, const scalar_type distThreshold);
        
        /// Return the descriptions of *this and its related Events.
        std::set<std::string> getDescriptions() const;
                
    protected:
        /// Event description.  Defined by an instance of IDCriterion, typically.
        std::string desc;
        /// Data value related to a specific event.
        scalar_type val;
        /// Location of *this in physical space.
        ll_coord_type latLon;
        /// DateTime of *this.
        DateTime datetime;
        /// Location of *this in data space.
        std::vector<index_type> dataIndex;
        /// Filename where *this was found.
        std::string filename;
        /// Time index in file where *this was found.
        index_type time_index;
        /// Related Events, defined by addRelated().
        std::vector<std::shared_ptr<Event>> relatedEvents;
        /// True if this is in another Event's relatedEvents vector.
        bool isReferenced;
        /// Type of Intensity comparison associated with *this
        IntensityComparison compare;
};

/// Equivalance implies exact duplicates: same event type, same event value, same location and same DateTime.
inline bool operator == (const Event& left, const Event& right) {return left.isDuplicate(right);}
/// Less than implies that the left event has lower intensity than the right event.
inline bool operator < (const Event& left, const Event& right) {return left.lowerIntensity(right); }
/// Greater than implies that the left event has higher intensity than the right event.
inline bool operator > (const Event& left, const Event& right) {return right < left; }

}
#endif
