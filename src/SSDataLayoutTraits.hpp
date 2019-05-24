#ifndef _SS_DATA_LAYOUT_TRAITS_HPP_
#define _SS_DATA_LAYOUT_TRAITS_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <array>
#include <vector>

namespace StrideSearch {

/// Templated struct defines traits and parameters related to the horizontal grid type of a netCDF dataset.
/**
    @note Users are not meant to use this class directly.  Instead, use the typedef specializations
    ::UnstructuredLayout and ::LatLonLayout.
    
    spatial_index_type is a horizontal index plus a vertical level index.
    netcdf_access_type is the type required for use with the netCDF 4 c++ API.
*/
template <int nHorizDims=1>
struct DataLayoutTraits {
    /// Type of a horizontal grid point's index employed by StrideSearch.
    typedef std::array<Index,nHorizDims> horiz_index_type;
    /// StrideSearch horizontal index plus a vertical leve index.
    typedef std::array<Index,nHorizDims+1> spatial_index_type;
    /// Data type required by the netCDF 4 c++ API.
    typedef std::vector<size_t> netcdf_access_type;
    
    /// Helper function.  Returns an empy netcdf_access_type for "flat" variables in a netCDF file.
    static netcdf_access_type get_index_2d() {
        return netcdf_access_type(nHorizDims+1,-1);
    }
    
    /// Helper function. Returns an empty netcdf_access_type for 3D variables in a netCDF file.
    static netcdf_access_type get_index_3d() {
        return netcdf_access_type(nHorizDims+2,-1);
    }
    
    /// Converts a tree index (output from nanoflann) to a data index.
    /**
        This function is used for UnstructuredLayout, where tree indices are equal to data indices.
    */
    static horiz_index_type getDataIndexFromTreeIndex(const Index tree_ind) {
        return horiz_index_type({tree_ind});}
     
    /// Converts a tree index (output from nanoflann) to a data index.    
    /**
        This function is used for LatLonLayout, and presumes the grid points are packed into the nanoflann
        API as defined by LatLonNCReader::makePoints.
    */
    static horiz_index_type getDataIndexFromTreeIndex(const Index tree_ind, const Index nlon) {
        return horiz_index_type({tree_ind/nlon, tree_ind%nlon});}
};

typedef DataLayoutTraits<1> UnstructuredLayout;
typedef DataLayoutTraits<2> LatLonLayout;

}
#endif
