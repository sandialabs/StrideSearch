#ifndef _STRIDE_SEARCH_DEFS_HPP_
#define _STRIDE_SEARCH_DEFS_HPP_

#include "StrideSearchConfig.h"
#include <array>
#include <iostream>

#ifdef HAVE_KOKKOS
#include "Kokkos_Core.hpp"
#else
#include <vector>
#endif

namespace StrideSearch {
    /// Floating point data type
    typedef double Real;
    
    /// Integer data type
    typedef int Int;
    
    /// Memory index data type
    typedef int Index;
    
#ifdef HAVE_KOKKOS
    typedef Kokkos::View<Real*> RealArray;
    typedef Kokkos::View<Index*> IndexArray;
#else
    /// Real array data type
    typedef std::vector<Real> RealArray;
    
    /// Index array data type
    typedef std::vector<Index> IndexArray;
#endif
}
#endif
