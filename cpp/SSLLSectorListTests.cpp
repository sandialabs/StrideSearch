#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchSectorList_LatLon.h"
#include "StrideSearchData_LatLon.h"
#include <vector>
#include <iostream>
#include <string>

using namespace StrideSearch;

int main (int argc, char* argv[]) {
    print_copyright();
    
    std::string dataDir(StrideSearch_TEST_DATA_DIR);
    //std::string dataDir("/Users/pabosle/Desktop/dataTemp/");
//     std::string testFile("OLI-ECMWF-SCM-RA_01Oct2016_dxdy10000dz40_qlsfsnd11_ice001_tauls7200.nc");
    //std::string testFile("f1850c5_ne240_rel06.cam.h2.0002-07-28-00000.nc");
    std::string testFile("sresa1b_ncar_ccsm3-example.nc");
    std::string inputFile = dataDir + "/" + testFile;
    
    std::cout << "Reading data from file : " << inputFile << std::endl;
    
    std::vector<std::string> search_vars = {"tas"};
    
    StrideSearchData_LatLon ncData(inputFile, search_vars);
    
    std::cout << "*** LLData info ***\n";
    std::cout << ncData.basicInfo();
    std::cout << "-------------------\n";
    
    const double sb = -40.0;
    const double nb = 40.0;
    const double wb = 0.0;
    const double eb = 360.0;
    const double radius = 3000.0;
    
    SectorList_LatLon ssectors(sb, nb, wb, eb, radius);
    std::cout << "sector list built; n = " << ssectors.nSectors() << std::endl;
    
    std::vector<ll_coord_type> sec_centers = ssectors.listSectorCenters();
    std::cout << "*** sector centers ***\n";
    for (int i = 0; i < ssectors.nSectors(); ++i) {
        std::cout << "(" << sec_centers[i].first << ", " << sec_centers[i].second << ")\n";
    }
    
return 0;
}
