#ifndef _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_
#define _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_

#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDataLatLon.h"

namespace StrideSearch {

/// Specialized SectorList for uniform latitude-longitude grids.
/**
    The assumed grid structure has the following properties:
    - nLongitude points = 2 * (nLatitude points - 1)
    - data resolution in radians is dLambda = 2 * PI / (nLongitude points)
    - longitude values are given by (j - 1) * dLambda for j = 1, 2, ..., nLongitude points.
    - latitude values are given by -PI/2.0 + (i - 1) * dLambda for i = 1, 2, ..., nLatitude points.
*/
class SectorListLatLon : public SectorList {
    public:
        SectorListLatLon(const scalar_type sb, const scalar_type nb, const scalar_type wb, const scalar_type eb, 
            const scalar_type sector_radius_km, index_type nLats, index_type nLons);
        SectorListLatLon(const std::vector<ll_coord_type>& secCenters, const std::vector<scalar_type>& radii);
        
        void linkSectorsToData();
    
        ~SectorListLatLon() {};

    protected:
        /// number of latitude points in grid
        index_type nLat;
        /// number of longitude points in grid
        index_type nLon;
        /// data resolution in radians
        scalar_type dLambda;
        /// data resolution in degrees
        scalar_type grid_res_deg;
        /// minimum latitudinal data index for a given search region
        index_type latMinIndex;
        /// maximum latitudinal data index for a given search region
        index_type latMaxIndex;
        /// minimum longitudinal data index for a given search region
        index_type lonMinIndex;
        /// maximum data index for a given search region
        index_type lonMaxIndex;
        /// latitude stride, in units of data indices
        index_type lat_stride_index;
        /// longitude strides, in units of data indices
        std::vector<index_type> lon_stride_indices;        
        
};

}
#endif
