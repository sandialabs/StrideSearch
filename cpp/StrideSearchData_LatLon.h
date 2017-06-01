#ifndef _STRIDE_SEARCH_DATA_LATLON_H_
#define _STRIDE_SEARCH_DATA_LATLON_H_

#include "StrideSearchUtilities.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspace.h"
#include <vector> 
#include <string>

namespace StrideSearch {

class StrideSearchData_LatLon : public StrideSearchData {
    public:
        StrideSearchData_LatLon(const std::string fname, const std::vector<std::string> varnames) : 
            StrideSearchData(fname, varnames) {
            initDimensions();
        };
        ~StrideSearchData_LatLon(){};
        
        void getGridDescription(index_type* gridDescInts) const;
        
        Workspace getSectorWorkingData(const std::vector<std::string>& crit_vars, 
            const std::vector<std::vector<index_type> >& dataIndices);

        void readFullFile(const std::string var);
    
        void read2DDataFromSingle(const std::string var, const index_type latIndex);
    
        void readFullWChunks(const index_type time_index);

        void read2DDataFromTimestep(const int time_index, const index_type level_index = 0);
        
        std::string basicInfo() const;
        
        double getDatumValue(const std::string var, const index_type latInd, const index_type lonInd);
        
        std::vector<std::pair<scalar_type, scalar_type> > getLLCoordsFromIndices(
            const std::vector<std::vector<index_type> >& dataIndices) const;
    
    protected:
        index_type nLat;
        index_type nLon;
        std::vector<scalar_type> lons;
        std::vector<scalar_type> lats;
        Workspace2D nc_data;
        
        void initDimensions();
};

}
#endif
