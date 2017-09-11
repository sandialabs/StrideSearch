#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDataBase.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchTimer.h"
#include "StrideSearchEvent.h"
#include "StrideSearchDateTime.h"
#include <string>
#include <iostream>
#include <vector>
#include <memory>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
    
    Timer setupTimer("TropicalCyclone_setup");
    setupTimer.start();
    //
    //  Set up data set for reading
    //
    const std::string data_dir = "/Users/pabosle/Desktop/dataTemp";
    //const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0002-08-27-00000.nc";
    const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0002-07-28-00000.nc";
    const std::string full_name = data_dir + "/" + data_filename;
    std::vector<std::string> file_list = {full_name};
    
    StrideSearchData ssData(file_list[0]);
    ssData.initDimensions();
    
    std::cout << ssData.infoString();
    
    //
    //  Set up search region and StrideSearch sectors
    //
    const scalar_type south_bnd = -30.0;  // southern boundary of search region (degrees north)
    const scalar_type north_bnd = 30.0; // northern boundary of search region (degrees north)
    const scalar_type west_bnd = 0.0; // western boundary of search region (degrees east)
    const scalar_type east_bnd = 360.0; // eastern boundary of search region (degrees east)
    
    const scalar_type sector_size_km = 500.0; // search sector radius size (kilometers)

    SectorList sectors(south_bnd, north_bnd, west_bnd, east_bnd, sector_size_km);
    sectors.linkSectorsToData(&ssData);
    
    std::cout << sectors.infoString(); 
    
    //
    //  Set up identification criteria
    //
    const std::string vort_varname = "VOR850";
    const scalar_type vort_threshold = 3.5e-4;
    
    MaxCriterion vor850(vort_varname, vort_threshold);
    
    const std::string psl_varname = "PSL";
    const scalar_type psl_threshold = 99000.0;
    
    MinCriterion psl(psl_varname, psl_threshold);
    
    const std::string mid_level_temp = "T500";
    const std::string upper_level_temp = "T200";
    const scalar_type warm_core_threshold = 2.0;
    MaxVariationCriterionVerticalAvg warm_core(mid_level_temp, upper_level_temp, warm_core_threshold);
    
    std::vector<IDCriterion*> id_criteria = {&vor850, &psl, &warm_core};
    
    // collocation criteria
    const scalar_type vort_psl_dist_threshold = 450.0; // km
    const scalar_type temp_psl_dist_threshold = 225.0; // km
    
    sectors.buildWorkspaces(id_criteria);
    
    setupTimer.end();
    std::cout << setupTimer.infoString();
    
    //
    // Run spatial search
    //
    Timer searchTimer("TropicalCylone_spatial_search");
    searchTimer.start();
    
    searchTimer.end();
    std::cout << searchTimer.infoString(); 
    
    std::vector<IDCriterion*> location_criterion(1, &vor850);
    
    typedef std::shared_ptr<Event> event_ptr_type;
    //
    //  Loop over each data file
    //  (note: this loop is embarrassingly parallel)
    //
    for (auto& file : file_list) {
        ssData.updateSourceFile(file);
        //
        //  Loop over each time step (note: this loop is embarrassingly parallel)
        //
        for (index_type k = 0; k < ssData.nTimesteps(); ++k) {
//             DateTime currentTime
            //
            //  Loop over each sector 
            //  (note: this loop is embarrassingly parallel, but its output must be 
            //  synced to prevent duplicates)
            //
            for (index_type i = 0; i < sectors.nSectors(); ++i) {
                
            }
        }
    }
return 0;
}