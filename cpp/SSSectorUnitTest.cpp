#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <vector>
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchSector.h"
#include "StrideSearchIDCriteria_Base.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchEvent.h"

using namespace StrideSearch;

int main() {
    //
    //  setup identification criteria
    //
    const std::vector<std::string> horizWindVarnames = {"u", "v"};
    const std::string slpvarname = "sea_level_pressure_hpa";
    
    MinCriterion slpCrit(slpvarname, 990.0);
    MaxMagnitude2DCriterion wndSpdCrit(horizWindVarnames, 22.0);
    
    std::vector<IDCriterion*> criteria = {&slpCrit, &wndSpdCrit};

    //
    //  create sample data 
    //
    const int nPoints = 10;
    std::vector<ll_coord_type> crds;
    std::vector<vec_indices_type> inds;
    vec_indices_type index(2,-1);
    std::vector<scalar_type> slp;
    std::vector<scalar_type> uu;
    std::vector<scalar_type> vv;
    for (int i = 0; i < nPoints; ++i) {
        crds.push_back(ll_coord_type(i - 5.0, 175.0 + i));
        index[0] = 90 + i;
        index[1] = 175 + i;
        inds.push_back(index);
        
        uu.push_back(20.0 + (i < 5 ? i : -i));
        vv.push_back(5.0 + (i < 5 ? 0.5 * i : -0.5 * i));
        slp.push_back(990.0 + (i%2 == 0 ? -i : 2*i));
    }
    
    const scalar_type dummy_radius = 1000.0;
    Sector emptySec(90.0, 0.0, dummy_radius, 0);
    std::cout << emptySec.infoString() << std::endl;
    
    Sector sec(0.0, 180.0, dummy_radius, crds, inds, criteria.size(), 0);
    sec.allocWorkspace(criteria);
    
    
    //
    //  create sample workspaces (in practice, data will be filled from StrideSearchData)
    //
    sec.workspace[0].fillData(slpvarname, slp);
    sec.workspace[1].fillData(horizWindVarnames[0], uu);
    sec.workspace[1].fillData(horizWindVarnames[1], vv);
    
    
    std::cout << sec.infoString() << std::endl;
    
    //
    //  evaluate criteria in sector
    //
    const DateTime codingDay(2017, 6, 15, 16);
    const index_type ind(20);
    const std::string fname = "placeholder.nc";
    std::vector<std::shared_ptr<Event>> events = sec.evaluateCriteriaAtTimestep(criteria, codingDay, fname, ind);
    
    for (int i = 0; i < events.size(); ++i) {
        std::cout << events[i]->infoString();
    }
    
return 0;
}
