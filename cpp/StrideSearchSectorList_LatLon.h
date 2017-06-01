#ifndef _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_
#define _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_

#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"

namespace StrideSearch {

class SectorList_LatLon : public SectorList {
    protected:
        index_type nLat;
        index_type nLon;
        scalar_type dLambda;
        scalar_type grid_res_deg;
        index_type latMinIndex;
        index_type latMaxIndex;
        index_type lonMinIndex;
        index_type lonMaxIndex;
        index_type lat_stride_index_type;
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
