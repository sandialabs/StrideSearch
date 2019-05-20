#include "StrideSearchConfig.h"
#include "SSNCReader.hpp"
#include "SSUtilities.hpp"
#include "SSWorkspace.hpp"
#include "SSConsts.hpp"
#include <iostream>
#include <exception>
#include <iomanip>
#include <cmath>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
print_copyright();
std::cout << "testing netCDF readers." << std::endl;
    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string conus_file = data_dir + "/conusx4v1.g";
    const std::string unif_file = data_dir + "/sresa1b_ncar_ccsm3-example.nc";
    
    std::cout << "Looking for unstructured grid data in file: " << conus_file << std::endl;
    UnstructuredNCReader conusncr(conus_file);
    auto conusPts = conusncr.makePoints();
    std::cout << "conus file: found " << conusncr.nPoints() << " horizontal grid points (expected 9907)." << std::endl;
    if (conusncr.nPoints() != 9907) {
        throw std::runtime_error("conus grid file read error: incorrect nNodes");
    }
    
    std::cout << "Looking for structured grid data in file: " << unif_file << std::endl;
    LatLonNCReader llncr(unif_file);
    auto unifPts = llncr.makePoints();
    const Int nlat = 128;
    const Int nlon = 256;
    std::cout << "unif. file: found " << llncr.nPoints() << " horizontal grid points (expected "<<nlat*nlon<<")." << std::endl;
    if (llncr.nPoints() != nlat*nlon) {
        throw std::runtime_error("sres.. file read error: incorrect nPoints");
    }
    
    std::cout << "unif_";
    llncr.printLats();
    std::cout << "unif_";
    llncr.printLons();
    
    auto timevals = llncr.getTime();
    std::cout << "time = [";
    for (int i=0; i<timevals.size(); ++i) {
        std::cout << timevals[i] << (i==timevals.size()-1 ? "]\n" : " ");
    }
    if (timevals[0] != 730135.5) {
        throw std::runtime_error("time variable read error.");
    }
    
    Workspace wspc("pr", 2);
    typedef typename LatLonLayout::horiz_index_type h_ind_type;
    h_ind_type ind0({0,0});
    h_ind_type ind1({0,1});
    const std::vector<h_ind_type> inds = {ind0, ind1};
    const Int time_ind = 0;
    llncr.fillWorkspaceData(wspc, inds, time_ind);
    std::cout << std::setprecision(20) << wspc << std::endl;
    const Real diff = wspc.data.at("pr")[0] - 1.091546e-06 ;
    if (std::abs(diff) > ZERO_TOL) {
        throw std::runtime_error("pr variable read error.");
    }
    
    std::cout << "tests pass." << std::endl;
return 0;
}
