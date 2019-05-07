#ifndef _STRIDE_SEARCH_NANOFLANN_TREE_H_
#define _STRIDE_SEARCH_NANOFLANN_TREE_H_

#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchDataBase.h"
#include "nanoflann.hpp"
#include "StrideSearchNanoflannAdaptor.h"
#include <memory>

namespace StrideSearch {
  ///A NanoflannTree builds and saves the kd-tree when nanoflann.hpp is available. 
  using namespace nanoflann;

  class NanoflannTree {
  public:
    /// Constructor that saves the StrideSearchData for kd-tree building 
    NanoflannTree(const std::shared_ptr<StrideSearchData> data_ptr);
    
    /// If nanoflann.hpp library exists build kd-tree.
    void buildTree();

    /// Variable for creating adaptor. 
    NanoflannAdaptor adaptor;

    /// kd-tree data structure. 
    std::unique_ptr<tree_type> search_tree;

  protected:
    /// StrideSearchData used for tree build. 
    const std::shared_ptr<StrideSearchData> data;
  };
}

#endif
