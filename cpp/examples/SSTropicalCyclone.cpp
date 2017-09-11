#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchDataBase.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchTimer.h"
#include <string>
#include <iostream>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    Timer setupTimer("TropicalCyclone_setup");
    setupTimer.start();
    //
    //  Set up data set for reading
    //
    const std::string data_dir = "/Users/pabosle/Desktop/dataTemp";
    //const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0002-08-27-00000.nc";
    const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0002-07-28-00000.nc";
    const std::string full_name = data_dir + "/" + data_filename;
    
    StrideSearchData ssData(full_name);
    ssData.initDimensions();
    
    std::cout << ssData.infoString();
    
    //
    //  Set up search region and StrideSearch sectors
    //
    const scalar_type south_bnd = -30.0;  // southern boundary of search region (degrees north)
    const scalar_type north_bnd = 30.0; // northern boundary of search region (degrees north)
    const scalar_type west_bnd = 0.0; // western boundary of search region (degrees east)
    const scalar_type east_bnd = 360.0; // eastern boundary of search region (degrees east)
    
    const scalar_type sector_size_km = 500.0; // search sector radius size (kilometers)

    SectorList sectors(south_bnd, north_bnd, west_bnd, east_bnd, sector_size_km);
    sectors.linkSectorsToData(&ssData);
    
    std::cout << sectors.infoString(); 
    
    //
    //  Set up identification criteria
    //
    const std::string vort_varname = "VOR850";
    const scalar_type vort_threshold = 3.5e-4;
    
    MaxCriterion vor850(vort_varname, vort_threshold);
    
    const std::string psl_varname = "PSL";
    const scalar_type psl_threshold = 99000.0;
    
    MinCriterion psl(psl_varname, psl_threshold);
    
    const std::string mid_level_temp = "T500";
    const std::string upper_level_temp = "T200";
    const scalar_type warm_core_threshold = 2.0;
    MaxVariationCriterionVerticalAvg(mid_level_temp, upper_level_temp, warm_core_threshold);
    
    // collocation criteria
    const scalar_type vort_psl_dist_threshold = 450.0; // km
    const scalar_type temp_psl_dist_threshold = 225.0; // km
    
    setupTimer.end();
    std::cout << setupTimer.infoString();
return 0;
}