#ifndef _SS_KDTREE_HPP_
#define _SS_KDTREE_HPP_

#include "StrideSearchConfig.h"
#include "SSNCReader.hpp"
#include "nanoflann.hpp"
#include <memory>
#include <cmath>

namespace StrideSearch {

namespace nf = nanoflann;

/// Points adaptor for nanoflann kdtree 
/**

    @note: this class is an implementation detail, not typically used by client applications

    This is the interface to the nanoflann KdTree package.
*/
struct PointsKDTreeAdaptor {
    typedef Real coord_t;
    static constexpr Int max_leaf_size = 20;
    
    const Points& pts;
    
    PointsKDTreeAdaptor(const Points& pts_) : pts(pts_) {}
    
    inline size_t kdtree_get_point_count() const {
        return pts.n; 
    }
    
    inline coord_t kdtree_get_pt(const size_t idx, const size_t dim) const {
        coord_t result;
        if (dim==0) result = pts.x[idx];
        else if (dim==1) result = pts.y[idx];
        else if (dim==2) result = pts.z[idx];
        return result;
    }
    
    template <class BBOX>
    bool kdtree_get_bbox(BBOX& ) const {return false;}
};

typedef PointsKDTreeAdaptor adaptor_type;

typedef nf::KDTreeSingleIndexAdaptor<nf::L2_Simple_Adaptor<Real,adaptor_type>,adaptor_type,3,Index> tree_type;

/// KDTree for spatial search
/**
    @note: this class is an implementation detail, not typically used by client applications

    A tree is built only once per search (since the whole dataset is assumed to use the same grid).
    It is used to link SSData to each SSSector by defining each SSSector's local data indices.
*/
class KDTree {
    public:
        KDTree(const NCReader* ncr);
        
        std::unique_ptr<tree_type> index;
        
    protected:
        Points pts;
        adaptor_type adaptor;
        
};

}
#endif