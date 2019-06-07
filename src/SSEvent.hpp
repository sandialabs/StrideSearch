#ifndef _SS_EVENT_HPP_
#define _SS_EVENT_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSDataLayoutTraits.hpp"
#include "SSDateTime.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSEventTraits.hpp"
#include <set>
#include <memory>

namespace StrideSearch {

// fwd declarations
template <typename DL> class SectorSet;
template <typename DL> class EventSet;
template <typename DL> class Track;
class IDCriterion; 

/// A record of an important (as deemed by IDCriterion::evaluate methods) event in the data.
/**
    An Event is a record of a the data and location associated with an instance of a set of identification criteria
    (a collection of IDCriterion subclasses) evaluating as True in a Sector.
    
    For example, if an IDCriterion is defined to identify vorticity maxima greater than a threshold, when it encounters
    such a value in the data, it creates an Event with a description like "VortMax", the data value, location in 
    physical space (lat-lon coords), location in time as a DateTime instance, location in data space,
    the filename that contains the Event, the time index in that file that corresponds to the Event's time, and the Event
    type enum.
    
    The Event class is one of the fundamental StrideSearch classes (along with Sector and IDCriterion).
*/
template <typename DataLayout=UnstructuredLayout>
class Event {
    public:
    typedef typename DataLayout::horiz_index_type horiz_index_type;
    typedef typename DataLayout::spatial_index_type spatial_index_type;
    
    template <typename DL> friend class EventSet;
    template <typename DL> friend class SectorSet;
    template <typename DL> friend class SearchManager;
    template <typename DL> friend class Track;
    
    
    /// Constructor for an Event in a horizontal data field
    /**
        @param desc_ : description of Event (e.g., output from IDCriterion::description())
        @param val : value associated with Event (e.g., computed by IDCriterion::evaluate)
        @param lat_ : latitude coordinate
        @param lon_ : longitude coordinate
        @param dt : DateTime of Event
        @param ind : horizontal grid point index
        @param time_ind_ : time index 
        @param fname : filename for source data
        @param icomp : One of ::IntensityComparison types
        @param sdep : One of ::SpatialDependence types
    */
    Event(const std::string& desc_, const Real val, const Real lat_, const Real lon_, 
        const DateTime& dt, const horiz_index_type& ind, const Index& time_ind_, 
        const std::string& fname, const IntensityComparison& icomp, const SpatialDependence& sdep) : 
        desc(desc_), value(val), lat(lat_), lon(lon_), datetime(dt), loc_ind(ind), loc_ind_3d(),
        time_ind(time_ind_), filename(fname), intensity_comparison(icomp),
        spatial_dependence(sdep), relatedEvents(), isReferenced(false) {}
    
    /// Constructor for an Event in a 3D data field
    /**
        @param desc_ : description of Event (e.g., output from IDCriterion::description())
        @param val : value associated with Event (e.g., computed by IDCriterion::evaluate)
        @param lat_ : latitude coordinate
        @param lon_ : longitude coordinate
        @param dt : DateTime of Event
        @param ind : horizontal grid point and level index
        @param time_ind_ : time index 
        @param fname : filename for source data
        @param icomp : One of ::IntensityComparison types
        @param sdep : One of ::SpatialDependence types
    */
    Event(const std::string& desc_, const Real val, const Real lat_, const Real lon_,
        const DateTime& dt, const spatial_index_type& ind, const Index& time_ind_, const std::string& fname,
        const IntensityComparison& icomp, const SpatialDependence& sdep) :
        desc(desc_), value(val), lat(lat_), lon(lon_), datetime(dt), loc_ind_3d(ind), loc_ind(), 
        time_ind(time_ind_), filename(fname), intensity_comparison(icomp),
        spatial_dependence(sdep), relatedEvents(), isReferenced(false) {}
    
    /// Description of the Event
    /**
        In most cases this will match the output of an IDCriterion::description() function.
    */
    inline std::string description() const {return desc;}
    
    /// Full Event data output
    /**
        @param tab_level : lowest indentation level for output
    */
    std::string infoString(const int tab_level=0) const;
    
    std::vector<std::shared_ptr<Event<DataLayout>>> flatten() const;
    
    /// Return the value
    inline Real getVal() const {return value;}
    
    /// Add a related Event to this Event.
    /**
        Related Events are different events that correspond to the same data feature.
        For example, a vorticity maxima may be associated with a related pressure minima.
        
        To require related Events to have different types, use EventSet methods EventSet::removeDuplicates and
        EventSet::consolidateRelated.
        
        @param relEv : shared pointer to a related Event
        @throws std::runtime_error if relEv and *this do not have the same DateTime.
    */
    void addRelated(std::shared_ptr<Event> relEv);
    
    /// True if other is less intense than *this.
    /**
        @param other : Event to compare with *this
        
        @throws std::runtime_error if *this and other are not the same type
    */
    bool lowerIntensity(const Event& other) const;
    
    /// Evalates True if other is an exact duplicate of this.
    /**
        Duplicates may occur when overlapping Sectors detect the same event.
        Duplicates imply literal duplicates -- the exact same data.
        
        @param other : Event to compare with *this
    */
    inline bool isDuplicate(const Event& other) const {
        return desc == other.desc && datetime == other.datetime && lat == other.lat && lon == other.lon;
    }
    
    /// Evaluates true if *this and other events are near each other, where near is defined as a distance threshold.
    /**
        @param other : Event to compare with *this
        @param dist : distance threshold; distances less than this value are considered "near."
    */
    bool isNear(const Event& other, const Real dist) const;
    
    /// True if *this and other have the same DateTime
    /**
        @param other : Event to compare with *this
    */
    inline bool sameDateTime(const Event& other) const {return datetime == other.datetime;}
    
    /// Evaluates true if two Events of the same type have locations that are near each other.  
    /**
        Redundant Events are events of the same type that are closely spaced; for example, two overlapping sectors may     
        find the same Event based on their own local data, but the data may be different between the two sectors.
        
        @param other : Event to compare with *this
        @param distThreshold : distance threshold in kilometers.
        @return distance(*this,other) < distThreshold
    */
    bool isRedundant(const Event& other, const Real distThreshold) const;
    
    /// The minimum distance separating *this and its related Events
    Real minRelatedDistance() const;
    
    /// The maximum distance separating *this and its related Events
    Real maxRelatedDistance() const;
    
    /// Geodesic distance betweeen *this and another Event.
    /**
        @param other : Another Event of any kind
        @return distance(*this, other) in kilometers
    */
    Real distance(const Event& other) const;
    
    /// True if *this and one of its related events are collocated in space.
    /**
        An output of "true" implies that *this and its related Events have exactly one record associated with 
        IDCriterion crit1 and exactly one record associated with IDCriterion crit2.  
        Furthermore, the locations of these two records are separated by a distance less than or equal to
        the distanceThreshold.
        
        Assumption 1: EventSet::consolidateRelated has already finished.@n
        Assumption 2: *this and its relatedEvents have the same time_index (this is enforced by Event::addRelated()).
    */
    bool isCollocated(const IDCriterion* crit1, const IDCriterion* crit2, const Real distThreshold) const;
    
    /// Return the descriptions of *this and its related Events.
    std::set<std::string> getDescriptions() const;
    
    /// Returns the (lat,lon) coordinates of this event in an array.
    inline std::array<Real,2> getCoords() const {return std::array<Real,2>({lat,lon});}
    
    inline int nRelated() const {return relatedEvents.size();}
    
    protected:
        /// Event description.  Defined by IDCriterion::description() typically.
        std::string desc;
        /// Latitude of Event
        Real lat;
        /// Longitude of Event
        Real lon;
        /// Datetime of Event
        DateTime datetime;
        /// Value of Event
        Real value; 
        /// Nearest location in data space (horizontal only)
        horiz_index_type loc_ind;
        /// Nearest location in data space (horizontal and level)
        spatial_index_type loc_ind_3d;
        /// Time index in data file
        Index time_ind;
        /// Filename for file containing the data that define this Event
        std::string filename;
        /// ::IntensityComparison trait
        IntensityComparison intensity_comparison;
        /// ::SpatialDependence trait
        SpatialDependence spatial_dependence;
        /// Events related to *this
        std::vector<std::shared_ptr<Event>> relatedEvents;
        /// True if *this is in another Event's relatedEvents vector.
        bool isReferenced;
        
        std::string mgrCSVString() const;
};

template <typename DataLayout>
inline bool operator == (const Event<DataLayout>& left, const Event<DataLayout>& right) {
    return left.isDuplicate(right);
}

template <typename DataLayout>
inline bool operator < (const Event<DataLayout>& left, const Event<DataLayout>& right) {
    return left.lowerIntensity(right);
}

template <typename DataLayout>
inline bool operator > (const Event<DataLayout>& left, const Event<DataLayout>& right) {
    return right < left;
}

}
#endif
