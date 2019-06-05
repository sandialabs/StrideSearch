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
  ///A NanoflannTree builds and saves the kd-tree when nanoflann.hpp is available. 

  #ifdef USE_NANOFLANN
  using namespace nanoflann;
  #endif

  class NanoflannTree {
  public:
    /// Constructor that saves the StrideSearchData for kd-tree building 
    NanoflannTree(const std::shared_ptr<StrideSearchData> data_ptr);
    
    /// If nanoflann.hpp library exists build kd-tree.
    void buildTree();

    #ifdef USE_NANOFLANN
    /// Type definition for kd-tree single index adaptor. 
    typedef nanoflann::KDTreeSingleIndexAdaptor<SphereDistAdaptor<scalar_type,NanoflannAdaptor>,NanoflannAdaptor,3,index_type>tree_type;

    /// Variable for creating adaptor. 
    NanoflannAdaptor* adaptor;

    /// kd-tree data structure. 
    tree_type* search_tree;
    #endif

  protected:
    /// StrideSearchData used for tree build. 
    const std::shared_ptr<StrideSearchData> data;
  };
}

#endif
