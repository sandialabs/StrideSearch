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
using namespace nanoflann;
template<class T, class DataSource, typename _DistanceType = T>
struct SphereDistAdaptor
{
    typedef T ElementType;
    typedef _DistanceType DistanceType;
    
    const DataSource& data_source;
    
    SphereDistAdaptor(const DataSource& _data_source) : data_source(_data_source) {};
    
    inline DistanceType evalMetric(const T* a, const size_t b_idx, size_t size) const {
        const T ax = a[0];
        const T ay = a[1];
        const T az = a[2];
        const T bx = data_source.kdtree_get_pt(b_idx, 0);
        const T by = data_source.kdtree_get_pt(b_idx, 1);
        const T bz = data_source.kdtree_get_pt(b_idx, 2);
        const T cp1 = ay * bz - by * az;
        const T cp2 = bx * az - ax * bz;
        const T cp3 = ax * by - bx * ay;
        const T cpnorm = std::sqrt(cp1 * cp1 + cp2 * cp2 + cp3 * cp3);
        const T dotprod = ax * bx + ay * by + az * bz;
        const T sph_dist = EARTH_RADIUS_KM * std::atan2(cpnorm, dotprod);
        return sph_dist * sph_dist;
    }
    
    template <typename U, typename V> 
    inline DistanceType accum_dist(const U a, const V b, int) const {
      return (b-a) * (b-a);
    }
};

struct metric_SphereDist : public Metric {
    template <class T, class DataSource>
    struct traits {
        typedef SphereDistAdaptor<T, DataSource> distance_t;
    };
};

struct NanoflannAdaptor {
    NanoflannAdaptor(const std::shared_ptr<StrideSearchData> ssdata) : data_ptr(ssdata) {};
    
    inline index_type kdtree_get_point_count() const {return data_ptr.lock()->nPoints();}
    
    inline scalar_type kdtree_get_pt(const index_type ind, const int dim) const {
        scalar_type lat;
        scalar_type lon;
        if (data_ptr.lock()->layout1d()) {
            lat = data_ptr.lock()->lats[ind];
            lon = data_ptr.lock()->lons[ind];
        }
        else {
            const std::pair<index_type, index_type> llind = data_ptr.lock()->get2dIndex(ind);
            lat = data_ptr.lock()->lats[llind.first];
            lon = data_ptr.lock()->lons[llind.second];
        }
        if (dim == 0) {
            return EARTH_RADIUS_KM * std::cos(lat * deg2rad) * std::cos(lon * deg2rad);
        }
        else if (dim == 1) {
            return EARTH_RADIUS_KM * std::cos(lat * deg2rad) * std::sin(lon * deg2rad);
        }
        else {
            return EARTH_RADIUS_KM * std::sin(lat * deg2rad);
        }
    };

    template <class BBOX> bool kdtree_get_bbox(BBOX&) const {return false;}
    
    std::weak_ptr<StrideSearchData> data_ptr;
};

}

#endif
