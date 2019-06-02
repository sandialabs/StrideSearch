#ifndef _STRIDE_SEARCH_SECTOR_LIST_BASE_H_
#define _STRIDE_SEARCH_SECTOR_LIST_BASE_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDataBase.h"
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchIDCriterionBase.h"
#include "StrideSearchSector.h"
#include "StrideSearchEventList.h"
#include "StrideSearchEvent.h"
#include "StrideSearchNanoflannTree.h"
#include <vector>
#include <memory>
#ifdef USE_NANOFLANN
#include "StrideSearchNanoflannAdaptor.h"
#endif

namespace StrideSearch {

/// A SectorList is the data structure used by driver programs to conduct searches of data sets.
/**
    SectorList is responsible for 
    1. Creating Sectors within a given search region defined as a rectangle
    in latitude-longitude space.
    2. Linking each sector to the data points and indices within the file associated with an StrideSearchData_Base subclass.
    3. Providing a control structure (loop) for the per-timestep search.
    
    @todo SectorList setup is slow.  Need an octree or k-d tree algorithm for linkSectorsToData.
*/
class SectorList {
    public:
        /// Constructor that defines its own Sectors. Inputs are the boundaries of a search region, and a sector radius in kilometers.
        /**
            @param sb southern latitudinal boundary, in degrees
            @param nb norhern latitudinal boundary, in degrees
            @param wb western longitudinal boundary, in degrees
            @param eb eastern longitudinal boundary, in degrees
            @param sector_radius_km sector geodesic radius, in kilometers
        */
        SectorList(scalar_type sb, scalar_type nb, scalar_type wb, scalar_type eb, scalar_type sector_radius_km);
        
        /// Constructor for known Sector centers.
        SectorList(const std::vector<ll_coord_type>& centers, const std::vector<scalar_type>& radii);
        
        /// Constructor for Sectors centered on a set of Events contained in EventList.
        SectorList(const EventList& evList, const scalar_type radius);
        
        virtual ~SectorList() {};
        
        /// Return the number of sectors in a SectorList
        inline int nSectors() const {return sectors.size();}
        
        /// Return a vector of all Sector centers.
        std::vector<ll_coord_type> listSectorCenters() const;
        
        /// Get basic or all information about a specific Sector, output to string.
        std::string sectorInfoString(const index_type secInd, const bool printAllData = false) const;
        
        /// Get basic information about *this SectorList, output to string.
        std::string infoString() const;
        
        /// Allocate memory and build workspaces for a common set of identification criteria across all sectors.
        void buildWorkspaces(const std::vector<IDCriterion*>& criteria);
        /// Allocate memory and build workspaces where each Sector has its own set of identification criteria.
        void buildWorkspaces(const std::vector<std::vector<IDCriterion*>>& separate_criteria);
        
        /// Links each sector to the data points within its boundaries.
        void linkSectorsToData(const std::shared_ptr<StrideSearchData> data_ptr, NanoflannTree tree);

	void linkSectorsToDataWNano(const std::shared_ptr<StrideSearchData> data_ptr);

	void linkSectorsToDataWOutNano(const std::shared_ptr<StrideSearchData> data_ptr);
        
        /// Find the sector that's closest to an arbitrary point.
        index_type closestSectorToPoint(const scalar_type lat, const scalar_type lon) const;


        
        /// Pointers to Sectors contained by *this
        std::vector<std::unique_ptr<Sector>> sectors;
        
        /// Returns the maximum number of data points contained by any one sector.
        index_type maxDataPointsPerSector() const;
        /// Returns the minimum number of data points contained by any one sector.
        index_type minDataPointsPerSector() const;

	//void initTree(const std::shared_ptr<StrideSearchData> data_ptr);
        
    protected: 
        /// Southern boundary of search domain, in [-90, 90).        
        scalar_type southBnd;
        /// Northern boundary of search domain, in (-90, 90].
        scalar_type northBnd;
        /// Western boundary of search domain, in [0, 360).
        scalar_type westBnd;
        /// Eastern boundary of search domain, in (0, 360].
        scalar_type eastBnd;
        /// radius of each Sector in *this.
        scalar_type radius;
        
        /// Latitude stride separating latitude strips.
        scalar_type lat_stride_deg;
        
        /// Number of latitude strips in *this, if applicable.
        index_type nStrips;
        
        /// Longitude strides along each latitude strip, if applicable.
        std::vector<scalar_type> lon_strides_deg;

	//#ifdef USE_NANOFLANN
	//NanoflannAdaptor* adaptor;
	//tree_type* search_tree;
        //#endif
	
};


}

#endif
