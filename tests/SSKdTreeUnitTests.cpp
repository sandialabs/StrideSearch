#include "SSKdTree.hpp"
#include "SSUtilities.hpp"
#include <iostream>
#include "nanoflann.hpp"
#include <cmath>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
print_copyright();
std::cout << "testing nanoflann interface." << std::endl;
    
    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string conus_file = data_dir + "/conusx4v1.g";
    const std::string unif_file = data_dir + "/sresa1b_ncar_ccsm3-example.nc";
    
    std::unique_ptr<NCReader> conus(new UnstructuredNCReader(conus_file));
    std::unique_ptr<NCReader> unif(new LatLonNCReader(unif_file));
    
    KDTree conusTree(conus.get());
    KDTree unifTree(unif.get());

    const size_t num_results=1;
    size_t ret_ind;
    Real sqdist;
    const Real qlat = 45.0;
    const Real qlon = 0.0;
    Real qx,qy,qz;
    llToXYZ(qx, qy, qz, qlat, qlon);
    const Real query_pt[3] = {qx, qy, qz};
    {
    const std::vector<std::pair<Index,Real>> searchReturn = conusTree.search(qlat, qlon, 500.0, 20);
    std::cout << "found " << searchReturn.size() << " data points near query pt in conus grid." << std::endl;

    for (Int i=0; i<searchReturn.size(); ++i) {
        std::cout << "found idx[" << i << "]= " << searchReturn[i].first << " dist[" << i << "]= " << std::sqrt(searchReturn[i].second) << " (lat,lon)= (" << conus->getLat(searchReturn[i].first) << "," << conus->getLon(searchReturn[i].first) << ")" << std::endl;
    } 
    std::cout << "________ csv ________" << std::endl;
    std::cout << "x,y,z" << std::endl;   
    for (Int i=0; i<searchReturn.size(); ++i) { 
        const Real lat = conus->getLat(searchReturn[i].first);
        const Real lon = conus->getLon(searchReturn[i].first);
        Real x,y,z;
        llToXYZ(x,y,z,lat,lon);
        std::cout << x/EARTH_RADIUS_KM << "," << y/EARTH_RADIUS_KM << "," << z/EARTH_RADIUS_KM << std::endl;
    }
    std::cout << "______ end csv _______" << std::endl;
    }
    {
    const std::vector<std::pair<Index,Real>> searchReturn = unifTree.search(qlat, qlon, 500.0, 20);
    std::cout << "found " << searchReturn.size() << " data points near query pt in lat-lon grid." << std::endl;
    for (Int i=0; i<searchReturn.size(); ++i) {
        std::cout << "found idx[" << i <<"]= " << searchReturn[i].first << " dist[" << i << "]= " << std::sqrt(searchReturn[i].second) << " (lat,lon)= (" << unif->getLat(searchReturn[i].first) << "," << unif->getLon(searchReturn[i].second) << ")" << std::endl;
    }
    }

std::cout << "tests pass." << std::endl;
return 0;
}
