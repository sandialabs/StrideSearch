#include "StrideSearchUtilities.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchData_LatLon.h"
#include "StrideSearchWorkspace.h"
#include <iostream>
#include <string>
#include <vector>

int main (int argc, char* argv[]) {
    print_copyright();
    
    std::string dataDir("/Users/pabosle/Desktop/dataTemp/");
    std::string testFile("f1850c5_ne240_rel06.cam.h2.0002-07-28-00000.nc");
    std::string inputFile = dataDir + testFile;
    
    std::vector<std::string> search_vars = {"PSL", "VOR850"};
    
    StrideSearchData_LatLon ncData(inputFile, search_vars);
    
    std::cout << ncData.basicInfo();
    
    const int time_index = 0;
    const int latInd = 200;
    const int lonInd = 801;
    ncData.read2DDataFromTimestep(time_index);
    std::cout << "PSL(" << time_index << ", " << latInd << ", " << lonInd << ") = " << 
        ncData.getDatumValue("PSL", latInd, lonInd) << " Pa\n";
    std::cout << "VOR850(" << time_index << ", " << latInd << ", " << lonInd << ") = " << 
        ncData.getDatumValue("VOR850", latInd, lonInd) << " 1/s\n";
        
    std::vector<std::vector<int> > rndDataInds;
    for (int k = 0; k < 5; ++k) {
        rndDataInds.push_back(std::vector<int>(2,0));
    }
    rndDataInds[0][0] = latInd;
    rndDataInds[0][1] = lonInd - 1;
    rndDataInds[1][0] = latInd;
    rndDataInds[1][1] = lonInd;
    rndDataInds[2][0] = latInd;
    rndDataInds[2][1] = lonInd + 1;
    rndDataInds[3][0] = latInd - 1;
    rndDataInds[3][1] = lonInd;
    rndDataInds[4][0] = latInd + 1;
    rndDataInds[4][1] = lonInd;
    
    Workspace wspc = ncData.getSectorWorkingData(search_vars, rndDataInds);
    std::cout << wspc;
    
    std::vector<std::pair<double,double> > secLL = ncData.getLLCoordsFromIndices(rndDataInds);
    std::cout << "lat-lon pts : VOR850 = \n";
    for (int i = 0; i < rndDataInds.size(); ++i) {
        std::cout << "(" << secLL[i].first << ", " << secLL[i].second << ") : ";
        std::cout << ncData.getDatumValue("VOR850", rndDataInds[i][0], rndDataInds[i][1]) << " 1/s\n";
    }
    
return 0;
}
