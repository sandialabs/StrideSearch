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
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>

#ifdef USE_NANOFLANN
#include "StrideSearchNanoflannTree.h"
#endif

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
    Timer programTimer("TropicalCylconeDriver");
    programTimer.start();
    //should be command line args. Namelist or some other way to read in data.     
    Timer setupTimer("TropicalCyclone_setup");
    setupTimer.start();
    //
    //  Set up data set for reading
    //
    const std::string data_dir = "/gscratch/pabosle/strideSearchData/matt/";
    const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0004-07-18-00000.nc";
    const std::string full_name = data_dir + "/" + data_filename;
    std::vector<std::string> file_list = {full_name};
    
    const DateTime startDate(1850, 10, 1, 0); // Data set initial date = October 1, f1850 compset.
    
    std::shared_ptr<StrideSearchData> ssData(new StrideSearchData(file_list[0]));
    ssData->initDimensions();

    
    std::cout << ssData->infoString();
    
    //
    //  Set up search region and StrideSearch sectors
    //
    const scalar_type south_bnd = -30.0;  // southern boundary of search region (degrees north)
    const scalar_type north_bnd = 30.0; // northern boundary of search region (degrees north)
    const scalar_type west_bnd = 0.0; // western boundary of search region (degrees east)
    const scalar_type east_bnd = 360.0; // eastern boundary of search region (degrees east)
    
    const scalar_type sector_size_km = 500.0; // search sector radius size (kilometers)

    SectorList sectors(south_bnd, north_bnd, west_bnd, east_bnd, sector_size_km);

    NanoflannTree tree(ssData); 

    tree.buildTree();

    sectors.linkSectorsToData(ssData,tree);
    
    std::cout << sectors.infoString(); 
    
    //
    //  Set up identification criteria
    //
    const std::string vort_varname = "VOR850";
    const scalar_type vort_threshold = 8.5e-4;
    
    MaxCriterion vor850(vort_varname, vort_threshold);
    
    const std::string psl_varname = "PSL";
    const scalar_type psl_threshold = 99000.0;
    
    MinCriterion psl(psl_varname, psl_threshold);
    
    const std::string mid_level_temp = "T500";
    const std::string upper_level_temp = "T200";
    const scalar_type warm_core_threshold = 2.0;
    MaxVariationCriterionVerticalAvg warm_core(mid_level_temp, upper_level_temp, warm_core_threshold);
    
    std::vector<IDCriterion*> loc_criteria = {&vor850};
    std::vector<IDCriterion*> id_criteria = {&vor850, &psl, &warm_core};
    
    // collocation criteria
    const scalar_type vort_psl_dist_threshold = 450.0; // km
    const scalar_type temp_psl_dist_threshold = 225.0; // km
    
    sectors.buildWorkspaces(loc_criteria);
    
    setupTimer.end();
    std::cout << setupTimer.infoString();
    
    //
    // Run spatial search
    //
    Timer searchTimer("TropicalCylone_spatial_search");
    searchTimer.start();    
    typedef std::shared_ptr<Event> event_ptr_type;
    EventList mainList;
    //
    //  Loop over each data file
    //  (note: this loop is embarrassingly parallel)
    //
    int filecounter = 0;
    std::stringstream ss;
    std::string nullstr;
    for (auto& file : file_list) {
        ssData->updateSourceFile(file);
        std::cout << "... processing file " << ++filecounter << " of " << file_list.size() << std::endl;
        //
        //  Loop over each time step (note: this loop is embarrassingly parallel)
        //
        ss << " of " << ssData->nTimesteps() << "; ";
        const std::string nTimestepsStr = ss.str();
        ss.str(nullstr);
        for (index_type k = 0; k < ssData->nTimesteps(); ++k) {
            const DateTime currentDate(ssData->getTime(k), startDate);
            std::cout << "timestep " << std::setw(3) << std::setfill(' ') << k << nTimestepsStr << currentDate.DTGString(); 
            //
            //  Loop over each sector, load data, use a single criterion to locate possible events
            //  (note: this loop is embarrassingly parallel)
            //
            std::vector<std::vector<event_ptr_type>> possibleEvents;
            int possCounter = 0;
            for (index_type i = 0; i < sectors.nSectors(); ++i) {
                ssData->loadSectorWorkingData(sectors.sectors[i].get(), k);
                possibleEvents.push_back(
                    sectors.sectors[i]->evaluateCriteriaAtTimestep(loc_criteria, currentDate, file, k));
                possCounter += possibleEvents[i].size();
            }
            //
            //  Create new sectors, one at each possible event location -- search for all criteria
            //
            EventList possibleList(possibleEvents);
            possibleList.removeDuplicates(sector_size_km);
            
            SectorList timestepSecList(possibleList, sector_size_km);
            timestepSecList.linkSectorsToData(ssData,tree);
            timestepSecList.buildWorkspaces(id_criteria);
            //
            //  Loop over possible event sectors, load data, evaluate all criteria 
            //    (note: this loop is embarrassingly parallel)
            //
            int evCounter = 0;
            std::vector<std::vector<event_ptr_type>> foundEvents;
            for (index_type i = 0; i < timestepSecList.nSectors(); ++i) {
                ssData->loadSectorWorkingData(timestepSecList.sectors[i].get(), k);
                foundEvents.push_back(
                    timestepSecList.sectors[i]->evaluateCriteriaAtTimestep(id_criteria, currentDate, file, k));
                evCounter += foundEvents[i].size();
            }
            EventList foundEventList(foundEvents);
            foundEventList.removeDuplicates(sector_size_km);
            //
            //  Apply collocation criteria
            //
            foundEventList.consolidateRelatedEvents(sector_size_km);
            foundEventList.requireCollocation(&vor850, &psl, vort_psl_dist_threshold);
            foundEventList.requireCollocation(&warm_core, &psl, temp_psl_dist_threshold);
            std::cout << " : " << std::setw(2) << std::setfill(' ') << evCounter << " events." << std::endl;     
            //
            //  Save timestep data
            //
            mainList.extend(foundEventList);
        }
    }
    searchTimer.end();
    
    //
    //  write output
    //
    std::ofstream outfile("sstropicalResults.txt");
    const int indent_tabs = 0;
    const bool printAll = true;
    outfile << mainList.infoString(indent_tabs, printAll);
    outfile.close();
    
    std::cout << setupTimer.infoString();
    
    std::cout << searchTimer.infoString(); 
    
    programTimer.end();
    std::cout << programTimer.infoString();
return 0;
}
