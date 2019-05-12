#ifndef _SS_SECTOR_HPP_
#define _SS_SECTOR_HPP_

#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include "SSEventTraits.hpp"
#include "SSIdCriteria.hpp"
#include "SSEvent.hpp"
#include "SSKdTree.hpp"
#include "SSWorkspace.hpp"
#include "SSDateTime.hpp"
#include <memory>
#include <iostream>
#include <vector>
#include <string>

namespace StrideSearch {

/// A Sector is the StrideSearch algorithm's basic unit of work.
/**
    A Sector has a center point on the sphere, and   
    it maintains a record of all data points that lie within its boundaries (physical coordinates and data locations).
    
    Sectors are responsible for 
    1. Receiving data from SSData objects at each time step
    2. Evaluating a set of identification criteria (a collection of IDCriterion subclass instances).
    
    The Sector struct is one of the fundamental StrideSearch data types (along with Event and IDCriterion).
*/
template <typename DataLayout=UnstructuredLayout> 
struct Sector {
    typedef typename DataLayout::horiz_index_type horiz_index_type;
    typedef typename DataLayout::full_index_type full_index_type;

    /// Center latitude of Sector
    Real lat;
    /// Center longitude of Sector
    Real lon;
    /// Radius of Sector
    Real radius;
    /// Latitudes of data points contained in Sector
    std::vector<Real> lats;
    /// Longitudes of data points contained in Sector
    std::vector<Real> lons;
    /// Horizontal gridpoint indices of data points contained in Sector
    std::vector<horiz_index_type> indices;
    /// One Workspace for each IDCriterion the Sector will evaluate
    std::vector<Workspace> workspaces;
    /// Index of latitude strip this Sector is on; see SectorSet.
    Int stripId;
    
    /// Complete constructor
    Sector(const Real clat, const Real clon, const Real rad, 
        const std::vector<Real>& lats_, const std::vector<Real>& lons_, 
        const std::vector<horiz_index_type>& inds, const Int nCrit, const Int sid) :
        lat(clat), lon(clon), radius(rad), lats(lats_), lons(lons_), indices(inds), 
        workspaces(nCrit), stripId(sid) {}
    
    /// Incomplete constructor; data points & indices, and criteria are not yet known.
    Sector(const Real clat, const Real clon, const Real rad, const Int sid) :
        lat(clat), lon(clon), radius(rad), lats(), lons(), indices(), workspaces(), stripId(sid) {}
    
    /// Allocates workspaces and memory for each workspace
    void allocWorkspaces(const std::vector<std::shared_ptr<IDCriterion>>& criteria);
    
    /// Returns the number of points contained by this Sector
    inline Int nDataPoints() const {return indices.size();}
    
    /// Complete record of Sector object, output to string
    std::string infoString(const int tab_lev=0, const bool printWorkspaces=false) const;
    
    /// Distance of an arbitrary point to Sector center
    Real distanceToCenter(const Real qlat, const Real qlon) const {
        return sphereDistance(lat,lon, qlat,qlon);
    }
    
    /// Evaluates a set criteria against a set of local data; returns an event for each criterion that evaluates true.
    std::vector<std::shared_ptr<Event<DataLayout>>> evaluateCriteriaAtTimestep(
        std::vector<std::shared_ptr<IDCriterion>>& criteria,
        const DateTime& dt, const std::string& fname, const Index time_ind) const;
};
}
#endif
