#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSSearchManager.hpp"
#include "SSIdCriteria.hpp"
#include "SSCollocCriteria.hpp"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
    
    typedef LatLonLayout Layout;
    typedef std::shared_ptr<IDCriterion> crit_ptr;
    typedef std::shared_ptr<CollocationCriterion> colloc_ptr;
    
    region_type region({-30.0, 30.0, 90.0, 180.0});
    Real srad = 500.0;
    
    const std::string data_dir = StrideSearch_TEST_DATA_DIR;
    const std::string test_file1 = data_dir + "/f1850c5_ne240_rel06.cam.h2.0002-08-27-00000.nc";
    const std::string test_file2 = data_dir + "/f1850c5_ne240_rel06.cam.h2.0002-09-26-00000.nc";
    const std::vector<std::string> fnames = {test_file1, test_file2};
    
    const DateTime start(1851, 10, 1, 0);
        
    const Real vort_threshold = 8.5e-4;
    crit_ptr vor850(new MaxSignedCriterion("VOR850", "lat", vort_threshold));
    
    const Real psl_threshold = 100000.0;
    crit_ptr psl(new MinCriterion("PSL", psl_threshold));

    const Real windspeed_threshold = 10.0;
    crit_ptr sfcwind(new MaxMagnitudeCriterion("UBOT", "VBOT", windspeed_threshold));

    const Real warm_core_threshold = 2.0;
    crit_ptr warmcore(new MaxVariationOfAverageCriterion("T200", "T500", warm_core_threshold));
    
    const std::vector<crit_ptr> criteria = {vor850, psl, warmcore, sfcwind};
    
    const Real psl_warmcore_distance_threshold = 225.0;
    colloc_ptr pslWarmCoreColloc(new CollocationCriterion(psl, warmcore, psl_warmcore_distance_threshold));

    const Real vort_psl_distance_threshold = 450.0;
    colloc_ptr pslVortColloc(new CollocationCriterion(vor850, psl, vort_psl_distance_threshold));
    
    const std::vector<colloc_ptr> colloc_criteria = {pslVortColloc, pslWarmCoreColloc};    

    
    SearchManager<Layout> search(region, srad);
    search.setStartDate(start);
    search.setInputFiles(fnames);
    search.defineCriteria(criteria, colloc_criteria);
    
    search.runSpatialSearch(4);
    
    std::ofstream csvfile("testOutput.txt");
    search.outputCSV(csvfile);
    csvfile.close();
    
return 0;
}
