#ifndef _STRIDE_SEARCH_SECTOR_LIST_BASE_H_
#define _STRIDE_SEARCH_SECTOR_LIST_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchIDCriteria_Base.h"
#include <vector>
#include <memory>

namespace StrideSearch {

/// A Sector is the StrideSearch algorithm's basic unit of work.
/**
    A Sector has a center point on the sphere, and a geodesic radius.  
    It maintains a record of all data points that lie within its boundaries (physical coordinates and data location).
    This record is created by the SectorList that constructed this sector.
    
    Sectors are responsible for 
    1. Receiving data from StrideSearchData_Base subclasses at each time step
    2. Evaluating a set of identification criteria (a collection of IDCriterion subclass instances).
    3. If the criteria evaluate as true, the Sector creates an Event record.
*/
struct Sector {
    double centerLat;
    double centerLon;
        
    std::vector<ll_coord_type> data_coords;
    std::vector<vec_indices_type> data_indices;      
    
    std::vector<WorkspaceDict> workspace;
    
    /// Constructor.
    /**
        @param cLat latitude, in degrees, of sector center
        @param cLon longitude, in degrees, of sector center
        @param cds coordinates of the data points contained within this Sector
        @param inds indices of the data points contained within this Sector
    */
    Sector(const scalar_type cLat, const scalar_type cLon, const std::vector<ll_coord_type>& cds, 
        const std::vector<vec_indices_type>& inds, const int nCriteria) :
        centerLat(cLat), centerLon(cLon), data_coords(cds), data_indices(inds), {};
        
    void BuildWorkspace(const std::vector<IDCriterion>& criteria);
};

/// A SectorList is the basic unit of work for a single search.
/**
    SectorList is responsible for 
    1. Creating Sectors within a given search region defined as a rectangle
    in latitude-longitude space.
    2. Linking each sector to the data points and indices within the file associated with an StrideSearchData_Base subclass.
    3. Providing a control structure (loop) for the per-timestep search.
*/
class SectorList {
    public:
        /// Constructor. Inputs are the boundaries of a search region, and a sector radius in kilometers.
        /**
            @param sb southern latitudinal boundary, in degrees
            @param nb norhern latitudinal boundary, in degrees
            @param wb western longitudinal boundary, in degrees
            @param eb eastern longitudinal boundary, in degrees
        */
        SectorList(double sb, double nb, double wb, double eb, double sector_radius_km);
        virtual ~SectorList() {};
        
        /// Return the number of sectors in a SectorList
        int nSectors() const {return sec_center_lats.size();}
        
        /// Return a vector of all Sector centers.
        std::vector<ll_coord_type> listSectorCenters() const;
        
    protected: 
        scalar_type southBnd;
        scalar_type northBnd;
        scalar_type westBnd;
        scalar_type eastBnd;
        scalar_type radius;
        
        scalar_type lat_stride_deg;
        std::vector<scalar_type> lon_strides_deg;
        std::vector<scalar_type> sec_center_lats;
        std::vector<scalar_type> sec_center_lons;
        
        std::vector<std::unique_ptr<Sector> > sectors;
    
    
        /// Defines Sectors within a search region.
        void buildSectorList();
        
        /// Links each sector to the data points within its boundaries.
        virtual void linkSectorsToData(StrideSearchData* ssdata_ptr) = 0;
        
};


}

#endif
