#ifndef _STRIDE_SEARCH_TYPE_DEFS_H_
#define _STRIDE_SEARCH_TYPE_DEFS_H_

#include "StrideSearch_Config.h"

#ifdef USE_NANOFLANN
#include "nanoflann.hpp"
#endif

namespace StrideSearch {

    typedef double scalar_type;
    typedef int index_type;

    typedef std::pair<scalar_type, scalar_type> ll_coord_type;
    typedef std::pair<index_type, index_type> ll_index_type;
    typedef std::vector<index_type> vec_indices_type;
    
#ifdef USE_NANOFLANN

#endif
}

#endif
