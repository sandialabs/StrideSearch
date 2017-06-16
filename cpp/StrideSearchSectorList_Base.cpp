#include "StrideSearchUtilities.h"
#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"
#include <vector>
#include <cmath>
#include <memory>
#include <iostream>
#include <sstream>

namespace StrideSearch {

SectorList::SectorList(const scalar_type sb, const scalar_type nb, const scalar_type wb, const scalar_type eb, 
    const scalar_type sector_radius_km) : southBnd(sb), northBnd(nb), westBnd(wb), eastBnd(eb), radius(sector_radius_km) 
{
    const scalar_type sector_arc_length(radius / EARTH_RADIUS_KM);
    nStrips = index_type(std::floor( (deg2rad * northBnd - deg2rad * southBnd) / sector_arc_length) + 1);
    lat_stride_deg = (northBnd - southBnd) / (nStrips - 1);

    lon_strides_deg = std::vector<scalar_type>(nStrips, -1.0);
    for (index_type i = 0; i < nStrips; ++i){
        const scalar_type cLat(southBnd + i * lat_stride_deg);
        if (std::abs(std::abs(cLat) - 90.0) > ZERO_TOL) 
            lon_strides_deg[i] = radius / (EARTH_RADIUS_KM * std::cos(deg2rad * cLat)) / deg2rad;
        else
            lon_strides_deg[i] = 360.0;
    }
    
    for (index_type i = 0; i < nStrips; ++i) {
        const scalar_type latI = southBnd + i * lat_stride_deg;
        const index_type nLonsThisStrip(std::ceil((eastBnd - westBnd) / lon_strides_deg[i]));
        for (index_type j = 0; j < nLonsThisStrip; ++j) {
            const scalar_type lonJ = westBnd + j * lon_strides_deg[i];
            sectors.push_back(std::unique_ptr<Sector>(new Sector(latI, lonJ, i)));
        }
    }
}

std::vector<ll_coord_type> SectorList::listSectorCenters() const {
    std::vector<ll_coord_type> result;
    for (index_type i = 0; i < sectors.size(); ++i)
        result.push_back(ll_coord_type(sectors[i]->centerLat, sectors[i]->centerLon));
    return result;
}

std::string SectorList::sectorInfoString(const index_type secInd) const {
    return sectors[secInd]->infoString();
}

std::string SectorList::infoString() const {
    std::ostringstream ss;
    ss << "Sector List Record: \n";
    ss << "nSectors = " << nSectors();
    ss << "\tnumber of latitude strips = " << nStrips << std::endl;
    ss << "\tlat stride (deg) = " << lat_stride_deg << std::endl;
    ss << "\tlon strides (deg) = ";
    for (index_type i = 0; i < lon_strides_deg.size() - 1; ++i)
        ss << lon_strides_deg[i] << ", ";
    ss << lon_strides_deg[lon_strides_deg.size() - 1] << std::endl;
    ss << std::endl << "-------------------" << std::endl;
    return ss.str();
}

}
