#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchSectorList_Base.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDataLatLon.h"
#include "StrideSearchSectorListLatLon.h"
#include <cmath>
#include <iostream>
#include <sstream>

namespace StrideSearch {

SectorListLatLon::SectorListLatLon(const scalar_type sb, const scalar_type nb, const scalar_type wb, const scalar_type eb, 
    const scalar_type sector_radius_km, const index_type nLats, const index_type nLons) : 
    SectorList(sb, nb, wb, eb, sector_radius_km),  nLat(nLats), nLon(nLons) {
    dLambda = 2.0 * PI / nLon;
    grid_res_deg = 360.0 / nLon;
    latMinIndex = index_type(std::floor( (sb + 90.0) / grid_res_deg));
    latMaxIndex = index_type(std::floor( (nb + 90.0) / grid_res_deg));
    lonMinIndex = index_type(std::floor( wb / grid_res_deg));
    lonMaxIndex = index_type(std::floor( eb / grid_res_deg));
    lat_stride_index = index_type(std::floor(lat_stride_deg / grid_res_deg));
    for (index_type i = 0; i < lon_strides_deg.size(); ++i) {
        lon_stride_indices.push_back(index_type(std::floor( lon_strides_deg[i] / grid_res_deg)) + 1);
    }
    
    linkSectorsToData();
}

SectorListLatLon::SectorListLatLon(const std::vector<ll_coord_type>& secCenters, const std::vector<scalar_type>& radii) :
    SectorList(secCenters, radii) {};

void SectorListLatLon::linkSectorsToData(){
    for (index_type i = 0; i < sectors.size(); ++i) {
        const index_type centerLatIndex((std::floor(sectors[i]->centerLat + 90.0) / grid_res_deg));
        const index_type centerLonIndex(std::floor(sectors[i]->centerLon / grid_res_deg));
        
        const index_type lonStrideInt = lon_stride_indices[sectors[i]->stripID];
        
        std::vector<scalar_type> sector_lats;
        std::vector<scalar_type> sector_lons;
        std::vector<index_type> sector_lat_indices;
        std::vector<index_type> sector_lon_indices;
        for ( index_type ii = std::max(latMinIndex, centerLatIndex - lat_stride_index);
              ii < std::min(latMaxIndex, centerLatIndex + lat_stride_index); ++ii) {
            if (centerLonIndex - lonStrideInt < 0.0 ) {
                //
                //  Sector crosses longitude = 0.0
                //
                for (index_type j = 0; j < centerLonIndex + lonStrideInt; ++j) {
                    sector_lat_indices.push_back(ii);
                    sector_lon_indices.push_back(j);
                    sector_lats.push_back( -90.0 + ii * grid_res_deg );
                    sector_lons.push_back( j * grid_res_deg );
                }
                for (index_type j = nLon - 1 + centerLonIndex - lonStrideInt; j < nLon; ++j) {
                    sector_lat_indices.push_back(ii);
                    sector_lon_indices.push_back(j);
                    sector_lats.push_back( -90.0 + ii * grid_res_deg );
                    sector_lons.push_back( j * grid_res_deg );
                }
            }
            else if (centerLonIndex + lonStrideInt > 360.0) {
                //
                //  Sector crosses longitude = 360.0
                //
                for (index_type j = centerLonIndex - lonStrideInt - 1; j < nLon; ++j) {
                    sector_lat_indices.push_back(ii);
                    sector_lon_indices.push_back(j);
                    sector_lats.push_back( -90.0 + ii * grid_res_deg );
                    sector_lons.push_back( j * grid_res_deg );
                }
                for (index_type j = 0; j < lonStrideInt + nLon - 1 - centerLonIndex; ++j) {
                    sector_lat_indices.push_back(ii);
                    sector_lon_indices.push_back(j);
                    sector_lats.push_back( -90.0 + ii * grid_res_deg );
                    sector_lons.push_back( j * grid_res_deg );
                }
            }
            else {
                for (index_type j = centerLonIndex - lonStrideInt; j < centerLonIndex + lonStrideInt; ++j) {
                    sector_lat_indices.push_back(ii);
                    sector_lon_indices.push_back(j);
                    sector_lats.push_back( -90.0 + ii * grid_res_deg );
                    sector_lons.push_back( j * grid_res_deg );
                }
            }
        }
        
        for (index_type k = 0; k < sector_lat_indices.size(); ++k) {
            const scalar_type dist = sphereDistance(sectors[i]->centerLat, sectors[i]->centerLon, 
                sector_lats[k], sector_lons[k]);
            if (dist <= radius) {
                sectors[i]->data_coords.push_back(ll_coord_type(sector_lats[k], sector_lons[k]));
                const vec_indices_type ind = {sector_lat_indices[k], sector_lon_indices[k]};
                sectors[i]->data_indices.push_back(ind);
            }
        }
    }
}


}

