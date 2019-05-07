#ifndef _SS_KDTREE_HPP_
#define _SS_KDTREE_HPP_

#include "StrideSearchConfig.h"
#include "SSNCReader.hpp"
#include "nanoflann.hpp"

namespace StrideSearch {

namespace nf = nanoflann;

template <typename Derived> 
struct PointsKDTreeAdaptor {
    typedef typename Derived::coord_t coord_t;
    
    const Derived& obj;
    
    PointsKDTreeAdaptor(const Derived& pts) : obj(pts) {}
    
    inline const Derived& derived() const {return obj;}
    
    inline size_t kdtree_get_point_count() const {
        return derived().n; 
    }
    
    inline coord_t kdtree_get_pt(const size_t idx, const size_t dim) const {
        coord_t result;
        if (dim==0) result = derived().x[idx];
        else if (dim==1) result = derived().y[idx];
        else if (dim==2) result = derived().z[idx];
        return result;
    }
    
    template <class BBOX>
    bool kdtree_get_bbox(BBOX& ) const {return false;}
};

typedef PointsKDTreeAdaptor<Points> adaptor_type;

typedef nf::KDTreeSingleIndexAdaptor<nf::L2_Simple_Adaptor<Real,adaptor_type>,adaptor_type,3> tree_type;

}
#endif