#include "StrideSearchUtilities.h"
#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"
#include <vector>
#include <cmath>
#include <memory>

namespace StrideSearch {

SectorList::SectorList(const double sb, const double nb, const double wb, const double eb, 
    const double sector_radius_km){
    southBnd = sb;
    northBnd = nb;
    westBnd = wb;
    eastBnd = eb;
    radius = sector_radius_km;
    
    buildSectorList();
}

void SectorList::buildSectorList() {
    std::cout << "DEBUG buildSectorList entry\n";
    const double sector_arc_length = radius / EARTH_RADIUS_KM;
    const int nStrips = (int)(std::floor( (deg2rad * northBnd - deg2rad * southBnd) / sector_arc_length)) + 1;
    lat_stride_deg = (northBnd - southBnd) / (nStrips - 1);

    lon_strides_deg = std::vector<double>(nStrips, -1.0);
    for (int i = 0; i < nStrips; ++i){
        const double cLat = southBnd + i * lat_stride_deg;
        if (std::abs(std::abs(cLat) - 90.0) > ZERO_TOL) 
            lon_strides_deg[i] = radius / (EARTH_RADIUS_KM * std::cos(deg2rad * cLat)) / deg2rad;
        else
            lon_strides_deg[i] = 2.0 * PI / deg2rad;
    }
    
    for (int i = 0; i < nStrips; ++i) {
        const double latI = southBnd + i * lat_stride_deg;
        const int nLonsThisStrip = (int)(std::ceil((eastBnd - westBnd) / lon_strides_deg[i]));
        for (int j = 0; j < nLonsThisStrip; ++j) {
            const double lonJ = westBnd + j * lon_strides_deg[i];
            sec_center_lats.push_back(latI);
            sec_center_lons.push_back(lonJ);
        }
    }
}

std::vector<ll_coord_type> SectorList::listSectorCenters() const {
    std::vector<ll_coord_type> result;
    for (int i = 0; i < sec_center_lats.size(); ++i)
        result.push_back(ll_coord_type(sec_center_lats[i], sec_center_lons[i]));
    return result;
}

}
