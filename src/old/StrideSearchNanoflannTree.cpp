#include "StrideSearchNanoflannTree.h"

namespace StrideSearch {

  NanoflannTree::NanoflannTree(const std::shared_ptr<StrideSearchData> data_ptr) : data(data_ptr){}

  void NanoflannTree::buildTree() {
    #ifdef USE_NANOFLANN
    adaptor = new NanoflannAdaptor(data);
    const int max_leaf_size = 10;
    nanoflann::KDTreeSingleIndexAdaptorParams params(max_leaf_size);
    search_tree = new tree_type(3,*adaptor,params);
    search_tree->buildIndex();
    std::cout<<"Tree built"<<std::endl;
    #endif
  }

}
