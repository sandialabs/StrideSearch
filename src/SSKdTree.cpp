#include "SSKdTree.hpp"
#include <cmath>

namespace StrideSearch {

KDTree::KDTree(const NCReader* ncr) : pts(ncr->makePoints()), adaptor(PointsKDTreeAdaptor(pts)) {
    const nf::KDTreeSingleIndexAdaptorParams treeParams(PointsKDTreeAdaptor::max_leaf_size);
    index = std::unique_ptr<tree_type>(new tree_type(3, adaptor, treeParams));
    index->buildIndex();
}

std::vector<std::pair<Index,Real>> KDTree::search(const Real clat, const Real clon, const Real radius_km, 
    const Int size_guess) const {
    std::vector<std::pair<Index, Real>> ret_matches;
    ret_matches.reserve(size_guess);
    nf::SearchParams params;
    Real qx, qy, qz;
    llToXYZ(qx,qy,qz, clat,clon);
    const Real query_pt[3] = {qx,qy,qz};
    const Index nFound = index->radiusSearch(&query_pt[0], searchRadiusInput(radius_km), ret_matches, params);
    return ret_matches;
}

}
