#ifndef _STRIDE_SEARCH_NANOFLANN_TREE_H_
#define _STRIDE_SEARCH_NANOFLANN_TREE_H_

#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchDataBase.h"
#ifdef USE_NANOFLANN
#include "nanoflann.hpp"
#include "StrideSearchNanoflannAdaptor.h"
#endif

namespace StrideSearch {

  #ifdef USE_NANOFLANN
  using namespace nanoflann;
  #endif

  class NanoflannTree {
  public:
    
    NanoflannTree(const std::shared_ptr<StrideSearchData> data_ptr);
    
    void buildTree();

    #ifdef USE_NANOFLANN
    typedef nanoflann::KDTreeSingleIndexAdaptor<SphereDistAdaptor<scalar_type,NanoflannAdaptor>,NanoflannAdaptor,3,index_type>tree_type;
    NanoflannAdaptor* adaptor;
    tree_type* search_tree;
    #endif

  protected:
    const std::shared_ptr<StrideSearchData> data;
  };
}

#endif
