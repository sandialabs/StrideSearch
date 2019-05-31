#ifndef _STRIDE_SEARCH_SECTOR_H_
#define _STRIDE_SEARCH_SECTOR_H_ 

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include <vector>
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchIDCriterionBase.h"
#include "StrideSearchEvent.h"
#include "StrideSearchDateTime.h"
#include <memory>
#include <iostream>

namespace StrideSearch {

/// A Sector is the StrideSearch algorithm's basic unit of work.
/**
    A Sector has a center point on the sphere, and   
    it maintains a record of all data points that lie within its boundaries (physical coordinates and data location).
    This record is created by the SectorList that constructed this sector.
    
    Sectors are responsible for 
    1. Receiving data from StrideSearchData_Base subclasses at each time step
    2. Evaluating a set of identification criteria (a collection of IDCriterion subclass instances).
    
    The Sector struct is one of the fundamental StrideSearch data types (along with Event and IDCriterion).
*/
struct Sector {
    /// Sector's center latitude.
    scalar_type centerLat;
    /// Sector's center longitude.
    scalar_type centerLon;
    /// Sector radius (km)
    scalar_type radius;
    
    /// (lat, lon) coordinates of data points contained by this Sector.
    std::vector<ll_coord_type> data_coords;
    /// Coordinates in data space of grid points contained by *this.
    std::vector<vec_indices_type> data_indices;
    
    /// The integer index of the latitude strip that this sector lies on, if applicable.
    index_type stripID;
    
    /// Local (to *this Sector) memory for evaluating sets of IDCriterion instances.
    std::vector<WorkspaceDict> workspace;
    
    /// Complete constructor.
    Sector(const scalar_type cLat, const scalar_type cLon, const scalar_type rad, const std::vector<ll_coord_type>& crds,
           const std::vector<vec_indices_type>& inds, const index_type nCriteria, const index_type sid) : 
           centerLat(cLat), centerLon(cLon), radius(rad), data_coords(crds), 
           data_indices(inds), workspace(nCriteria), stripID(sid) {};
    
    /// Incomplete constructor.  Data points and indices are not known yet.
    Sector(const scalar_type cLat, const scalar_type cLon, const scalar_type rad, const index_type sid) : 
        centerLat(cLat), centerLon(cLon), radius(rad), stripID(sid) {};
    
    /// Prepares Sector's workspace for memory allocation
    void defineWorkspace(const std::vector<IDCriterion*>& criteria);
    
    /// Allocates memory for this sector's local workspace
    void allocWorkspace(const std::vector<IDCriterion*>& criteria);
    
    /// Returns the number of grid points contained by *this.
    inline index_type nDataPoints() const {return data_indices.size();}
    
    /// Evaluates a set criteria against a set of local data; returns an event for each criterion that evaluates true.
    std::vector<std::shared_ptr<Event>> evaluateCriteriaAtTimestep(std::vector<IDCriterion*>& criteria, const DateTime& dt, 
        const std::string& fname, const index_type timeIndex);
    
    /// Returns a string containing this sector's information.
    std::string infoString(const int tabLevel = 0, const bool printWspc = false) const;
    
    /// Returns the geodesic distance of an arbitrary point to *this Sector's center.
    scalar_type distanceToSectorCenter(const scalar_type lat, const scalar_type lon) const;
    
    /// Writes the physical coordinates contained by *this to a .csv file.
    void outputCoordsToCSV(std::ostream& os) const;
};


}

#endif