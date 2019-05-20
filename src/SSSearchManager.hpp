#ifndef _STRIDE_SEARCH_SEARCH_MANAGER_HPP_
#define _STRIDE_SEARCH_SEARCH_MANAGER_HPP_

#include "SSDefs.hpp"
#include "StrideSearchConfig.h"
#include "SSUtilities.h"
#include "SSDataLayoutTraits.hpp"
#include "SSNCReader.hpp"
#include "SSWorkspace.hpp"
#include "SSSector.hpp"

namespace StrideSearch {

template <typename DataLayout=UnstructuredLayout>
class SearchManager {
    public:
        
    
    protected:
        std::shared_ptr<NCReader> reader;
        KDTree tree;
};

}

#endif
