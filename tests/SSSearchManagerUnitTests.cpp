#include "StrideSearchConfig.h"
#include "SSSearchManager.hpp"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include <iostream>
#include <exception>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
std::cout << "testing SearchManager class..." << std::endl;

    region_type region({0, 30, 100, 130});
    const Real radius = 1000.0;
    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string conus_file = data_dir + "/conusx4v1.g";
    const std::string unif_file = data_dir + "/sresa1b_ncar_ccsm3-example.nc";
    
    const DateTime coding_day(2019, 5, 27, 18);
    
    SearchManager<UnstructuredLayout> usearch(region, radius);
    std::vector<std::string> unst_files = {conus_file};
    usearch.setStartDate(coding_day);
    usearch.setInputFiles(unst_files);
    std::cout << "UNSTRUCTURED LAYOUT" << std::endl;
    std::cout << usearch.infoString();
    std::cout << "-------------------" << std::endl;
    
    SearchManager<LatLonLayout> llsearch(region, radius);
    std::vector<std::string> ll_files = {unif_file};
    llsearch.setStartDate(coding_day);
    llsearch.setInputFiles(ll_files);
    std::cout << "LATLON LAYOUT" << std::endl;
    std::cout << llsearch.infoString();
    std::cout << "-------------------" << std::endl;

std::cout << "tests pass." << std::endl;
return 0;
}

