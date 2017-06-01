#ifndef _STRIDE_SEARCH_SECTOR_LIST_BASE_H_
#define _STRIDE_SEARCH_SECTOR_LIST_BASE_H_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchData_Base.h"
#include <vector>
#include <memory>

namespace StrideSearch {

struct Sector {
    double centerLat;
    double centerLon;
        
    std::vector<ll_coord_type> data_coords;
    std::vector<vec_indices_type> data_indices;      
    
    Sector(scalar_type cLat, scalar_type cLon, std::vector<ll_coord_type> cds, std::vector<vec_indices_type> inds) :
        centerLat(cLat), centerLon(cLon), data_coords(cds), data_indices(inds) {};
};

class SectorList {
    public:
        SectorList(double sb, double nb, double wb, double eb, double sector_radius_km);
        virtual ~SectorList() {};
        
        int nSectors() const {return sec_center_lats.size();}
        
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
    
        void buildSectorList();
        virtual void linkSectorsToData(StrideSearchData* ssdata_ptr) = 0;
        
};


}

#endif
