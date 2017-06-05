#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchData_LatLon.h"
#include "StrideSearchWorkspace.h"
#include <iostream>
#include <string>
#include <vector>
#include <ctime>

using namespace StrideSearch;

int main (int argc, char* argv[]) {
    print_copyright();
    
    std::string dataDir(StrideSearch_TEST_DATA_DIR);
    std::string testFile("sresa1b_ncar_ccsm3-example.nc");
    std::string inputFile = dataDir + "/" + testFile;
    
    std::vector<std::string> search_vars = {"tas", "ua"};
    
    StrideSearchData_LatLon ncData(inputFile, search_vars);
    
    std::cout << ncData.basicInfo();
    
    const int time_index = 0;
    const int level_index = 2;
    const int latInd = 100;
    const int lonInd = 200;
    ncData.read2DDataFromTimestep(time_index, level_index);
    std::cout << "tas(" << time_index << ", " << latInd << ", " << lonInd << ") = " << 
        ncData.getDatumValue("tas", latInd, lonInd) << " Pa\n";
    std::cout << "ua(" << time_index << ", " << latInd << ", " << lonInd << ") = " << 
        ncData.getDatumValue("ua", latInd, lonInd) << " 1/s\n";
        
    std::vector<ll_index_type> rndDataInds;
    rndDataInds.push_back(ll_index_type(latInd, lonInd-1));
    rndDataInds.push_back(ll_index_type(latInd, lonInd));
    rndDataInds.push_back(ll_index_type(latInd, lonInd+1));
    rndDataInds.push_back(ll_index_type(latInd-1, lonInd));
    rndDataInds.push_back(ll_index_type(latInd+1, latInd));

    
    Workspace1D wspc = ncData.getSectorWorkingData(search_vars, rndDataInds);
    std::cout << wspc;
    
    std::vector<std::pair<double,double> > secLL = ncData.getLLCoordsFromIndices(rndDataInds);
    std::cout << "lat-lon pts : VOR850 = \n";
    for (int i = 0; i < rndDataInds.size(); ++i) {
        std::cout << "(" << secLL[i].first << ", " << secLL[i].second << ") : ";
        std::cout << ncData.getDatumValue("tas", rndDataInds[i].first, rndDataInds[i].second) << " 1/s\n";
    }
    std::clock_t start;
    double duration;
    start = std::clock();
    // for(int i = 0; i < 10000; i++){
    //   ncData.read2DDataFromSingle("tas");
    // }
    ncData.readFullFile("tas");
    duration = (std::clock()-start)/(double)CLOCKS_PER_SEC;
    std::cout<<"Single: "<<duration<<"\n";

    start = std::clock();
    // for(int i = 0; i < 10000; i++){
    //   ncData.read2DDataFromTimestep(time_index, level_index);
    // }
    ncData.readFullWChunks(time_index);
    duration = (std::clock()-start)/(double)CLOCKS_PER_SEC;
    std::cout<<"Chunk: "<<duration<<"\n";


return 0;
}
