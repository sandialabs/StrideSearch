#ifndef _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_
#define _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_

#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"

namespace StrideSearch {

/// Specialized SectorList for uniform latitude-longitude grids.
class SectorList_LatLon : public SectorList {
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
        index_type lat_stride_index_type;
        /// longitude strides, in units of data indices
        std::vector<index_type> lon_stride_index_types;
        
        void linkSectorsToData(StrideSearchData* ssdata_ptr);
        
    public:
        SectorList_LatLon(const scalar_type sb, const scalar_type nb, const scalar_type wb, const scalar_type eb, 
            const scalar_type sector_radius_km) :
            SectorList(sb, nb, wb, eb, sector_radius_km) {};
        ~SectorList_LatLon() {};
        
};

}
#endif
