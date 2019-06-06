#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSNCReader.hpp"
#include "SSSector.hpp"
#include "SSWorkspace.hpp"
#include "SSIdCriteria.hpp"
#include <iostream>
#include <memory>
#include <string>
#include <vector>

using namespace StrideSearch;

int main(int argc, char* argv[]) {

    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string unif_file = data_dir + "/f1850c5_ne240_rel06.cam.h2.0002-09-26-00000.nc";
    
    const std::string zonal_wind_avg = "U250";
    
    Sector<LatLonLayout> sec(45.0, 0.0, 1000.0,-1);
    std::shared_ptr<NCReader> unif(new LatLonNCReader(unif_file));
    KDTree unifTree(unif.get());

    sec.linkToData(unifTree, unif);
    
//     std::cout << sec.infoString();

    std::shared_ptr<IDCriterion> arith_avg_crit(new MaxAverageCriterion(zonal_wind_avg,0));
    std::shared_ptr<IDCriterion> space_avg_crit(new MaxAverageCriterion(zonal_wind_avg, "area_weight", 0));
    std::vector<std::shared_ptr<IDCriterion>> criteria = {arith_avg_crit, space_avg_crit};
    
    sec.allocWorkspaces(criteria);
    
    for (auto& wspc : sec.workspaces) {
        unif->fillWorkspaceData(wspc, sec.indices, 0);
    }
    const DateTime dt(1851,10,1,0);
    auto results = sec.evaluateCriteriaAtTimestep(criteria, dt, "f1850c5_ne240_rel06.cam.h2.0002-09-26-00000.nc", 0);
    
    for (const auto& ev : results) {
        std::cout << ev->infoString();
        
    }
    
//     std::cout << sec.infoString(0,true);
        
return 0;
}
