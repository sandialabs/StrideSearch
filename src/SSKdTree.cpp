#include "SSKdTree.hpp"

namespace StrideSearch {

KDTree::KDTree(const NCReader* ncr) : pts(ncr->makePoints()), adaptor(PointsKDTreeAdaptor(pts)) {
    const nf::KDTreeSingleIndexAdaptorParams treeParams(PointsKDTreeAdaptor::max_leaf_size);
    index = std::unique_ptr<tree_type>(new tree_type(3, adaptor, treeParams));
    index->buildIndex();
}


}
