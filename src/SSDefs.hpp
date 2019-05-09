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
    
    /// Compile-time specification for data layout.
    template <int d>
    struct HorizontalDimensionType {
        enum {value = d};
        typedef std::array<Index,d> data_index_type;
    };
    
    /// Unstructured data layout instantiation.
    typedef HorizontalDimensionType<1> UnstructuredLayout;
    
    /// Structured lat-lon grid data layout instantiation.
    typedef HorizontalDimensionType<2> LatLonLayout;
    
//     std::ostream& operator << (std::ostream& os, const std::array<Index,1>& a) { 
//         os << a[0]; 
//         return os;
//     }
//     
//     std::ostream& operator << (std::ostream& os, const std::array<Index,2>& a) { 
//         os << "(" << a[0] << "," << a[1] << ")"; 
//         return os;
//     }
    
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
