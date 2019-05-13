#ifndef _STRIDE_SEARCH_DATA_LATLON_H_
#define _STRIDE_SEARCH_DATA_LATLON_H_

#include "StrideSearchUtilities.h"
#include "StrideSearchDataBase.h"
#include <vector> 
#include <string>

namespace StrideSearch {

/// Specialized data access for uniform lat-lon grids.
/**
    Note: this class is currently unused, except for its unit test.  
    @todo Should we keep this class?
*/
class StrideSearchDataLatLon : public StrideSearchData {
    public:
        /// Constructor
        StrideSearchDataLatLon(const std::string fname) : StrideSearchData(fname) {
            initDimensions();
        };
        
        /// Destructor
        ~StrideSearchDataLatLon(){};
        
        /// Return an integer array = [nLat, nLon]
        void getGridDescription(index_type* gridDescInts) const override;
        
        /// Return a string with basic information from the data file.
        std::string basicInfo() const;
        
        /// Read one scalar value from a data set.
        scalar_type getDatumValue(const std::string var, const index_type latInd, const index_type lonInd);
        
        /// Return a vector of lat-lon coordinates corresponding to a vector of lat-lon data indices.
        std::vector<ll_coord_type > getLLCoordsFromIndices(
            const std::vector<vec_indices_type>& dataIndices) const;
        
        /// Load data from file into a Sector's local workspace
        /**
            Assumes variables have dimensions (time, level, lat, lon) or (time, lat, lon)
        */
        void loadSectorWorkingData(Sector* sec, const index_type& tInd, const index_type& levInd = -1) override;
        
        /// Basic info about this StrideSearchData instance, output to a string.
        std::string infoString() const;
        
    protected:
        /// Number of meridional grid points per longitude line.
        index_type nLat;
        
        /// Number of zonal grid points per latitude line.
        index_type nLon;
        
        
        /// Allocates/reads dimension variables from .nc file.
        /**
            On output, latitude coordinate values and longitude coordinate values are filled into class member variables.
            
            @todo Add support for the case where nc variable names are not "lat" and "lon," e.g., "latitude"?  
        */
        void initDimensions() override;
};

}
#endif
