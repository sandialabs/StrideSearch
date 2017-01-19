#ifndef _STRIDE_SEARCH_DATA_LATLON_H_
#define _STRIDE_SEARCH_DATA_LATLON_H_

#include "StrideSearchUtilities.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchWorkspace.h"
#include <vector> 
#include <string>

class StrideSearchData_LatLon : public StrideSearchData {
    public:
        StrideSearchData_LatLon(const std::string fname, const std::vector<std::string> varnames) : 
            StrideSearchData(fname, varnames) {
            initDimensions();
        };
        ~StrideSearchData_LatLon(){};
        
        void getGridDescription(int* gridDescInts) const;
        
        Workspace getSectorWorkingData(const std::vector<std::string>& crit_vars, 
            const std::vector<std::vector<int> >& dataIndices);

        void read2DDataFromTimestep(const int time_index);
        
        std::string basicInfo() const;
        
        double getDatumValue(const std::string var, const int latInd, const int lonInd);
        
        std::vector<std::pair<double,double> > getLLCoordsFromIndices(const std::vector<std::vector<int> >& dataIndices) const;
    
    protected:
        int nLat;
        int nLon;
        std::vector<double> lons;
        std::vector<double> lats;
        Workspace2D nc_data;
        
        void initDimensions();
};


#endif
