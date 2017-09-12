#include "StrideSearchUtilities.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchDataBase.h"
#include <vector>
#include <cmath>
#include <memory>
#include <iostream>
#include <sstream>
#include <cassert>
#include <limits>

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
    
    std::cout << "SectorList setup:" << std::endl;
    std::cout << "\tnStrips = " << nStrips << std::endl;
    std::cout << "\tlatStride = " << lat_stride_deg << std::endl;
    for (index_type i = 0; i < lon_strides_deg.size() - 1; ++i){
        std::cout << lon_strides_deg[i] << ", ";
    }
    std::cout << lon_strides_deg[lon_strides_deg.size() - 1] << std::endl;
    
    
    for (index_type i = 0; i < nStrips; ++i) {
        const scalar_type latI = southBnd + i * lat_stride_deg;
        const index_type nLonsThisStrip(std::ceil((eastBnd - westBnd) / lon_strides_deg[i]));
        for (index_type j = 0; j < nLonsThisStrip; ++j) {
            const scalar_type lonJ = westBnd + j * lon_strides_deg[i];
            sectors.push_back(std::unique_ptr<Sector>(new Sector(latI, lonJ, radius, i)));
        }
    }
}

SectorList::SectorList(const std::vector<ll_coord_type>& centers, const std::vector<scalar_type>& radii) {
    for (index_type i = 0; i < centers.size(); ++i) {
        sectors.push_back(std::unique_ptr<Sector>(new Sector(centers[i].first, centers[i].second, radii[i], -1)));
    }
}

SectorList::SectorList(const EventList& evList, const scalar_type radius) {
    for (int i = 0; i < evList.size(); ++i) {
        const ll_coord_type loc = evList.getEvent(i)->location();
        sectors.push_back(std::unique_ptr<Sector>(new Sector(loc.first, loc.second, radius, -1)));
    }
}

#ifdef USE_NANOFLANN
    void SectorList::fastLinkSectorsToData(const StrideSearchData* data_ptr){};
#endif

void SectorList::linkSectorsToData(const StrideSearchData* data_ptr) {
    if (data_ptr->layout1d()) {
        //
        //  This loop is embarrassingly parallel
        //
        for (index_type sec_i = 0; sec_i < sectors.size(); ++sec_i) {
            for (index_type i = 0; i < data_ptr->lats.size(); ++ i) {
                const scalar_type dist = sphereDistance(sectors[sec_i]->centerLat, sectors[sec_i]->centerLon,
                    data_ptr->lats[i], data_ptr->lons[i]);
                if ( dist <= sectors[sec_i]->radius) {
                    sectors[sec_i]->data_coords.push_back(ll_coord_type(data_ptr->lats[i], data_ptr->lons[i]));
                    const std::vector<index_type> ind = {i};
                    sectors[sec_i]->data_indices.push_back(ind);
                }
            }
        }
    }
    if (data_ptr->layout2d()) {
        //
        //  This loop is embarrassingly parallel
        //
        for (index_type sec_i = 0; sec_i < sectors.size(); ++sec_i) {
            for (index_type i = 0; i < data_ptr->lats.size(); ++i) {
               for (index_type j = 0; j < data_ptr->lons.size(); ++j) {
                    const scalar_type dist = sphereDistance(sectors[sec_i]->centerLat, sectors[sec_i]->centerLon,
                        data_ptr->lats[i], data_ptr->lons[j]);
                    if ( dist <= sectors[sec_i]->radius ) {
                        sectors[sec_i]->data_coords.push_back(ll_coord_type(data_ptr->lats[i], data_ptr->lons[j]));
                        const std::vector<index_type> ind = {i, j};
                        sectors[sec_i]->data_indices.push_back(ind);
                    }         
               } 
            }
        }
    }
}

index_type SectorList::closestSectorToPoint(const scalar_type lat, const scalar_type lon) const {
    index_type result = -1;
    scalar_type dist = std::numeric_limits<scalar_type>::max();
    for (index_type i = 0; i < sectors.size(); ++i) {
        const scalar_type testDist = sectors[i]->distanceToSectorCenter(lat, lon);
        if (testDist < dist) {
            dist = testDist;
            result = i;
        }
    }
    return result;
}

index_type SectorList::maxDataPointsPerSector() const {
    index_type result = 0;
    for (index_type i = 0; i < sectors.size(); ++i) {
        const index_type nPts = sectors[i]->nDataPoints();
        if ( nPts > result) {
            result = nPts;
        }
    }
    return result;
}

index_type SectorList::minDataPointsPerSector() const {
    index_type result = std::numeric_limits<index_type>::max();
    for (index_type i = 0; i < sectors.size(); ++i) {
        const index_type nPts = sectors[i]->nDataPoints();
        if (nPts < result) {
            result = nPts;
        }
    }
    return result;
}

void SectorList::buildWorkspaces(const std::vector<IDCriterion*>& criteria) {
    for (index_type i = 0; i < sectors.size(); ++i) {
        sectors[i]->defineWorkspace(criteria);
        sectors[i]->allocWorkspace(criteria);
    }
}

void SectorList::buildWorkspaces(const std::vector<std::vector<IDCriterion*>>& separate_criteria) {
    assert(separate_criteria.size() == sectors.size());
    for (int i = 0; i < sectors.size(); ++i) {
        sectors[i]->defineWorkspace(separate_criteria[i]);
        sectors[i]->allocWorkspace(separate_criteria[i]);
    }
}

std::vector<ll_coord_type> SectorList::listSectorCenters() const {
    std::vector<ll_coord_type> result;
    for (index_type i = 0; i < sectors.size(); ++i)
        result.push_back(ll_coord_type(sectors[i]->centerLat, sectors[i]->centerLon));
    return result;
}

std::string SectorList::sectorInfoString(const index_type secInd, const bool printAllData) const {
    return sectors[secInd]->infoString(0, printAllData);
}

std::string SectorList::infoString() const {
    std::ostringstream ss;
    ss << "Sector List Record: \n";
    ss <<"\tsearch region SW corner (lat, lon) = (" << southBnd << ", " << westBnd << ")" << std::endl;
    ss <<"\tsearch region NE corner (lat, lon) = (" << northBnd << ", " << eastBnd << ")" << std::endl;
    ss << "nSectors = " << nSectors();
    if (lon_strides_deg.size() > 0) {
        ss << "\tnumber of latitude strips = " << nStrips << std::endl;
        ss << "\tlat stride (deg) = " << lat_stride_deg << std::endl;
        ss << "\tlon strides (deg) = ";
        for (index_type i = 0; i < lon_strides_deg.size() - 1; ++i)
            ss << lon_strides_deg[i] << ", ";
        ss << lon_strides_deg[lon_strides_deg.size() - 1] << std::endl;
    }
    ss << "\tmin pts per sector = " << minDataPointsPerSector() << std::endl;
    ss << "\tmax pts per sector = " << maxDataPointsPerSector() << std::endl;
    ss << std::endl << "-------------------" << std::endl;
    return ss.str();
}

}
