#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <vector>
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchSector.h"
#include "StrideSearchSectorListLatLon.h"
#include "StrideSearchIDCriteria_Base.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchEvent.h"
#include "StrideSearchData_Base.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchDataLatLon.h"
#include <string>
#include <vector>
#include <memory>

using namespace StrideSearch;

int main() {
    const std::string testfilename = "sresa1b_ncar_ccsm3-example.nc";
    std::string file = StrideSearch_TEST_DATA_DIR;
    file += "/";
    file += testfilename;
    std::cout << "looking for file : " << file << std::endl;
    //
    //  variable names must be in file
    //
    const std::string uvarname = "ua"; // zonal wind, 850 hPa
    const std::string tempvarname = "tas"; // surface temp
    const std::string precipvarname = "pr"; // precipitation flux
    
    StrideSearchDataLatLon ssData(file);
    
    std::cout << ssData.infoString();
    
    //
    //  pick an arbitrary time/level
    //
    const int time_index = 0;
    const int lev_index = 2;
    
    //
    //  define sectors
    //
    std::vector<ll_coord_type> sec_centers;
    const ll_coord_type dSecCenter(-5.0, 130.0);
    const ll_coord_type antArcCenter(-80.0, 90.0);
    const ll_coord_type nLantCenter(50.0, 330.0);
    
    sec_centers.push_back(dSecCenter);
    sec_centers.push_back(nLantCenter);
    sec_centers.push_back(antArcCenter);
    const std::vector<scalar_type> radii = {400.0, 800.0, 600.0};
    
    SectorListLatLon secList(sec_centers, radii);
    
    std::cout << "SectorList built, nSectors = " << secList.nSectors() << std::endl;
    secList.SectorList::linkSectorsToData(&ssData);
    
    std::cout << secList.infoString();
    
    
    //
    //  define id criterias
    //    
    MaxCriterion windCrit(uvarname, 20.0);
    MinCriterion tempCrit(tempvarname, 210.0);
    MaxCriterion precipCrit(precipvarname, 0.0002);

    std::vector<IDCriterion*> criteria1(1, &precipCrit);
    std::vector<IDCriterion*> criteria2(1, &windCrit);
    std::vector<IDCriterion*> criteria3(1, &tempCrit);
    
    std::vector<std::vector<IDCriterion*>> separateCriteria = {criteria1, criteria2, criteria3};
    
    secList.buildWorkspaces(separateCriteria);
    
    //
    //  load data from file
    //
    for (int i = 0; i < secList.nSectors(); ++i) {
        ssData.loadSectorWorkingData(secList.sectors[i].get(), time_index, lev_index);
    }
    
    for (int i = 0; i < secList.nSectors(); ++i)
        std::cout << secList.sectorInfoString(i, true);

    //
    //  evaluate criteria
    //
    const DateTime codingDay(2017, 6, 19, 12);
    std::vector<std::vector<std::shared_ptr<Event> > > foundEvents;
    for (int i = 0; i < secList.nSectors(); ++i) {
        foundEvents.push_back(
            secList.sectors[i]->evaluateCriteriaAtTimestep(separateCriteria[i], codingDay, ssData.getFilename(), time_index));
    }
    
    std::cout << "foundEvents.size() = " << foundEvents.size() << std::endl;
    for (int i = 0; i < foundEvents.size(); ++i) {
        if (foundEvents[i].size() > 0) 
            std::cout << "Event List " << i << ":\n";
        for (int j = 0; j < foundEvents[i].size(); ++j) {
            std::cout << foundEvents[i][j]->infoString();
        }
    }
return 0;
}