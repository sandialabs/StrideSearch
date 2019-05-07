#ifndef _STRIDE_SEARCH_NANOFLANN_ADAPTOR_H_
#define _STRIDE_SEARCH_NANOFLANN_ADAPTOR_H_

#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "nanoflann.hpp"
#include "StrideSearchDataBase.h"
#include <memory>
#include <cmath>

namespace StrideSearch {
namespace nf = nanoflann;

inline scalar_type chordLengthFromRadius(const scalar_type radius) {
    const scalar_type theta = radius / (2*PI*EARTH_RADIUS_KM);
    return std::sqrt(EARTH_RADIUS_KM*EARTH_RADIUS_KM*(1-2*std::cos(theta)));
}

struct SSDataAdaptor {
    typedef scalar_type coord_t;
    
    const StrideSearchData& data;
    
    SSDataAdaptor(const StrideSearchData& ssd) : data(ssd) {}
    
    inline size_t kdtree_get_point_count() const {
        return data.nPoints();
    }
    
    inline coord_t kdtree_get_pt(const size_t idx, const size_t dim) const {
        index_type lat_ind;
        index_type lon_ind;
        if (data.layout1d()) {
            lat_ind = idx;
            lon_ind = idx;
        }
        else if (data.layout2d()) {
            auto inds = data.get2dIndex(idx);
            lat_ind = inds.first;
            lon_ind = inds.second;
        }
        if (dim == 0) {
            return EARTH_RADIUS_KM*std::cos(data.lons[lon_ind])*std::cos(data.lats[lat_ind]);
        }
        else if (dim == 1) {
            return EARTH_RADIUS_KM*std::sin(data.lons[lon_ind])*std::cos(data.lats[lat_ind]);
        }
        else {
            return EARTH_RADIUS_KM*std::sin(data.lats[lat_ind]);
        }
    }
    
    template <class BBOX>
    bool kdtree_get_bbox(BBOX&) const {return false;}
};

typedef nf::KDTreeSingleIndexAdaptor<nf::L2_Simple_Adaptor<scalar_type, SSDataAdaptor>,
    SSDataAdaptor, 3> tree_type;
}
#endif
