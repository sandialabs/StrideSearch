#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchDataBase.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchMinMaxCriteria.h"
#include <string>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    //
    //  Set up search region and StrideSearch sectors
    //
    const scalar_type south_bnd = -30.0;  // southern boundary of search region (degrees north)
    const scalar_type north_bnd = 30.0; // northern boundary of search region (degrees north)
    const scalar_type west_bnd = 0.0; // western boundary of search region (degrees east)
    const scalar_type east_bnd = 360.0; // eastern boundary of search region (degrees east)
    
    const scalar_type sector_size_km = 500.0; // search sector radius size (kilometers)

    SectorList sectors(south_bnd, north_bnd, east_bnd, west_bnd, sector_size_km);
    
    //
    //  Set up data set for reading
    //
    const std::string data_dir = "/Users/pabosle/Desktop/dataTemp";
    const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0002-08-27-00000.nc";
    const std::string full_name = data_dir + "/" + data_filename;
    
    StrideSearchData ssData(full_name);
    
    //
    //  Set up identification criteria
    //
    const std::string vort_varname = "VOR850";
    const scalar_type vort_threshold = 3.5e-4;
    
    MaxCriterion vor850(vort_varname, vort_threshold);
    
    const std::string psl_varname = "PSL";
    const scalar_type psl_threshold = 99000.0;
    
    MinCriterion psl(psl_varname, psl_threshold);
    
    
return 0;
}