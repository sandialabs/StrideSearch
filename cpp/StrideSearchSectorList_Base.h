#ifndef _STRIDE_SEARCH_SECTOR_LIST_BASE_H_
#define _STRIDE_SEARCH_SECTOR_LIST_BASE_H_

#include "StrideSearchData_Base.h"
#include <vector>
#include <memory>

typedef std::pair<double, double> ll_coord_type;
typedef std::vector<int> indices_type;

struct Sector {
    double centerLat;
    double centerLon;
        
    std::vector<ll_coord_type> data_coords;
    std::vector<indices_type> data_indices;      
    
    Sector(double cLat, double cLon, std::vector<ll_coord_type> cds, std::vector<indices_type> inds) :
        centerLat(cLat), centerLon(cLon), data_coords(cds), data_indices(inds) {};
};

class SectorList {
    public:
        SectorList(double sb, double nb, double wb, double eb, double sector_radius_km);
        virtual ~SectorList() {};
        
        int nSectors() const {return sec_center_lats.size();}
        
        std::vector<ll_coord_type> listSectorCenters() const;
        
    protected: 
        double southBnd;
        double northBnd;
        double westBnd;
        double eastBnd;
        double radius;
        
        double lat_stride_deg;
        std::vector<double> lon_strides_deg;
        std::vector<double> sec_center_lats;
        std::vector<double> sec_center_lons;
        
        std::vector<std::unique_ptr<Sector> > sectors;
    
        void buildSectorList();
        virtual void linkSectorsToData(StrideSearchData* ssdata_ptr) = 0;
        
};



#endif
