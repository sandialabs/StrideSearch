#ifndef _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_
#define _STRIDE_SEARCH_SECTOR_LIST_LAT_LON_H_

#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"

class SectorList_LatLon : public SectorList {
    protected:
        int nLat;
        int nLon;
        double dLambda;
        double grid_res_deg;
        int latMinIndex;
        int latMaxIndex;
        int lonMinIndex;
        int lonMaxIndex;
        int lat_stride_int;
        std::vector<int> lon_stride_ints;
        
        void linkSectorsToData(StrideSearchData* ssdata_ptr);
        
    public:
        SectorList_LatLon(const double sb, const double nb, const double wb, const double eb, 
            const double sector_radius_km) :
            SectorList(sb, nb, wb, eb, sector_radius_km) {};
        ~SectorList_LatLon() {};
        
};

#endif
