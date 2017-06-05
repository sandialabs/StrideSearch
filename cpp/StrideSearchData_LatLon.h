#ifndef _STRIDE_SEARCH_DATA_LATLON_H_
#define _STRIDE_SEARCH_DATA_LATLON_H_

#include "StrideSearchUtilities.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspace.h"
#include <vector> 
#include <string>

namespace StrideSearch {

/// Specialized data access for uniform lat-lon grids.
/**
*/
class StrideSearchData_LatLon : public StrideSearchData {
    public:
        /// Constructor
        StrideSearchData_LatLon(const std::string fname, const std::vector<std::string> varnames) : 
            StrideSearchData(fname, varnames) {
            initDimensions();
        };
        
        /// Destructor
        ~StrideSearchData_LatLon(){};
        
        /// Return an integer array = [nLat, nLon]
        void getGridDescription(index_type* gridDescInts) const;
        
        /// Get local sector working data
        Workspace1D getSectorWorkingData(const std::vector<std::string>& crit_vars, 
            const std::vector<ll_index_type>& dataIndices);

        void readFullFile(const std::string var);
    
        void read2DDataFromSingle(const std::string var, const index_type latIndex);
    
        void readFullWChunks(const index_type time_index);

        void read2DDataFromTimestep(const int time_index, const index_type level_index = 0);
        
        /// Return a string with basic information from the data file.
        std::string basicInfo() const;
        
        /// Read one scalar value from a data set.
        double getDatumValue(const std::string var, const index_type latInd, const index_type lonInd);
        
        /// Return a vector of lat-lon coordinates corresponding to a vector of lat-lon data indices.
        std::vector<ll_coord_type > getLLCoordsFromIndices(
            const std::vector<ll_index_type >& dataIndices) const;
    
    protected:
        index_type nLat;
        index_type nLon;
        std::vector<scalar_type> lons;
        std::vector<scalar_type> lats;
        Workspace2D nc_data;
        
        /// Allocates/reads dimension variables from .nc file.
        /**
            On output, latitude coordinate values and longitude coordinate values are filled into class member variables.
            
            @todo Add support for the case where nc variable names are not "lat" and "lon," e.g., "latitude"?  
        */
        void initDimensions();
};

}
#endif
