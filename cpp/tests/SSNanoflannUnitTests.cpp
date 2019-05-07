#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchNanoflannAdaptor.h"
#include "StrideSearchDataBase.h"
#include "nanoflann.hpp"
#include <memory>

using namespace StrideSearch;

void dump_mem_usage();

int main(int argc, char* argv[]) {
    print_copyright();
    
    const int nlon = 180;
    auto unif = std::shared_ptr<StrideSearchData>(new StrideSearchData(nlon));
    std::cout << "unif mesh: " << unif->infoString();
    
    const scalar_type sectorSize = 500.0; // kilometers
    const scalar_type nf_radius = chordLengthFromRadius(sectorSize);
    
    const scalar_type query_lon = 0;
    const scalar_type query_lat = 45;
    scalar_type query_pt[3];
    
    llToXYZ(query_pt[0], query_pt[1], query_pt[2], query_lat, query_lon);
    

    SSDataAdaptor nfinterface(*unif);
    tree_type index(3, nfinterface, nf::KDTreeSingleIndexAdaptorParams(20 /* max leaf */));
    index.buildIndex();

    const size_t num_results = 1;
    size_t ret_index;
    scalar_type out_dist_sq;
    nanoflann::KNNResultSet<scalar_type> resultSet(num_results);
    resultSet.init(&ret_index, &out_dist_sq);
    index.findNeighbors(resultSet, &query_pt[0], nanoflann::SearchParams(20));
    
    std::cout << "knnSearch(nn="<<num_results<<"): =\n";
    std::cout << "ret_index = " << ret_index << " sqrt(out_dist_sq) = " << std::sqrt(out_dist_sq) << std::endl;
    std::cout << "lat[ret_index] = " << unif->lats[ret_index] << " lon[ret_index] = " << unif->lons[ret_index] << std::endl;
return 0;
}


