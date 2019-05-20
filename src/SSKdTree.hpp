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

    @note this class is an implementation detail, not typically used by client applications

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
        
        std::vector<std::pair<Index, Real>> search(const Real clat, const Real clon, const Real radius_km, const Int size_guess = 20) const;
        
        /// Convert a great-circle distance (km) on the sphere to the corresponding nanoflann input value
        /**
            1. nanoflann uses Euclidean distance, not great circle distance.  
               This function uses the law of cosines to convert a great circle into a chord length.
            2. To avoid extra cost of a sqrt, nanoflann uses the squared distance as its input parameter;
               this function also computes the squared distance.
               
            See nanoflann issue #94. 
            
            @param radius_km : Search radius, great circle distance in km
            @return squared Euclidean distance
        */
        inline Real searchRadiusInput(const Real radius_km) const {
            const Real theta = radius_km / EARTH_RADIUS_KM;
//             std::cout << "theta = " << theta << std::endl;
            return 2*EARTH_RADIUS_KM*EARTH_RADIUS_KM*(1-std::cos(theta));
        }
    protected:
        Points pts;
        adaptor_type adaptor;
        
};

}
#endif