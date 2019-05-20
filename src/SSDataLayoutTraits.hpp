#ifndef _SS_DATA_LAYOUT_TRAITS_HPP_
#define _SS_DATA_LAYOUT_TRAITS_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <array>
#include <vector>

namespace StrideSearch {

template <int nHorizDims=1>
struct DataLayoutTraits {
    typedef std::array<Index,nHorizDims> horiz_index_type;
    typedef std::array<Index,nHorizDims+1> spatial_index_type;
    typedef std::vector<size_t> netcdf_access_type;
    
    static netcdf_access_type get_index_2d() {
        return netcdf_access_type(nHorizDims+1,-1);
    }
    static netcdf_access_type get_index_3d() {
        return netcdf_access_type(nHorizDims+2,-1);
    }
    
    static horiz_index_type getDataIndexFromTreeIndex(const Index tree_ind) {
        return horiz_index_type({tree_ind});}
        
    static horiz_index_type getDataIndexFromTreeIndex(const Index tree_ind, const Index nlon) {
        return horiz_index_type({tree_ind/nlon, tree_ind%nlon});}
};

typedef DataLayoutTraits<1> UnstructuredLayout;
typedef DataLayoutTraits<2> LatLonLayout;


}
#endif