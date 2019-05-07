#include "StrideSearchNanoflannTree.h"

namespace StrideSearch {

  NanoflannTree::NanoflannTree(const std::shared_ptr<StrideSearchData> data_ptr) : data(data_ptr), adaptor(data_ptr) {
    buildTree();
  }

  void NanoflannTree::buildTree() {
    const int max_leaf_size = 20;
    nanoflann::KDTreeSingleIndexAdaptorParams params(max_leaf_size);
    search_tree = std::unique_ptr<tree_type>(new tree_type(3,adaptor,params));
    search_tree->buildIndex();
  }

}
