#include "SSKdTree.hpp"
#include <iostream>
#include "nanoflann.hpp"
#include <cmath>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    
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
    nf::KNNResultSet<Real> resultSet(num_results);
    resultSet.init(&ret_ind, &sqdist);
    std::cout << "searching for point nearest (lat,lon) = (45,0)" << std::endl;
    conusTree.index->findNeighbors(resultSet, &query_pt[0], nf::SearchParams(10));
    
    std::cout << "conus search results(nn="<<num_results<<")"<<std::endl;
    std::cout << "\tret_ind = " << ret_ind << " dist = " << std::sqrt(sqdist) << std::endl;
    std::cout << "\tpt found (lat,lon) = (" << conus->getLat(ret_ind) << "," << conus->getLon(ret_ind) << ")" << std::endl;
    }
    {
    nf::KNNResultSet<Real> resultSet(num_results);
    resultSet.init(&ret_ind, &sqdist);
    unifTree.index->findNeighbors(resultSet,&query_pt[0], nf::SearchParams(10));
    std::cout << "latlon search results(nn="<<num_results<<")"<<std::endl;
    std::cout << "\tret_ind = " << ret_ind << " dist = " << std::sqrt(sqdist) << std::endl;
    std::cout << "\tpt found (lat,lon) = (" << unif->getLat(ret_ind) << "," << unif->getLon(ret_ind) << ")" << std::endl;
    }
    
    
    
    
std::cout << "tests pass." << std::endl;
return 0;
}