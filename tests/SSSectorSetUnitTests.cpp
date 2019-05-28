#include "SSDataLayoutTraits.hpp"
#include "SSSectorSet.hpp"
#include "SSUtilities.hpp"
#include <iostream>
#include <exception>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
print_copyright();
std::cout << "Testing SectorSet class." << std::endl;

    const Real fake_data_res_degrees = 10.0;
    const Int nlat = 19;
    const Int nlon = 36;
    const Real wb = 100.0;
    const Real eb = 190.0;
    const Real sb = -30.0;
    const Real nb = 30.0;
    const Real rad = 2200.0;    
    
    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string conus_file = data_dir + "/conusx4v1.g";
    const std::string unif_file = data_dir + "/sresa1b_ncar_ccsm3-example.nc";

    std::shared_ptr<NCReader> conus(new UnstructuredNCReader(conus_file));
    std::shared_ptr<NCReader> unif(new LatLonNCReader(unif_file));
    KDTree conusTree(conus.get());
    KDTree unifTree(unif.get());
    
    SectorSet<UnstructuredLayout> usectors(sb, nb, wb, eb, rad);
    usectors.linkToData(conusTree, conus);
    std::cout << usectors.infoString();
    
    SectorSet<LatLonLayout> llsectors(sb, nb, wb, eb, rad);
    llsectors.linkToData(unifTree, unif);
    std::cout << llsectors.infoString();

std::cout << "tests pass." << std::endl;
return 0;
}
