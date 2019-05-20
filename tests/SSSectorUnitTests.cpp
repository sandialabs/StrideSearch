#include "SSSector.hpp"
#include "SSDefs.hpp"
#include "SSEventTraits.hpp"
#include "SSEvent.hpp"
#include "SSIdCriteria.hpp"
#include "SSUtilities.hpp"
#include "SSWorkspace.hpp"
#include <iostream>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
print_copyright();
std::cout << "Testing SS Sector class." << std::endl;

    const Real dummy_radius = 1000.0;
    Sector<> emptySec(90.0, 0.0, dummy_radius, 0);
    std::cout << "--- empty sector ---" << std::endl;
    std::cout << emptySec.infoString(1) << std::endl;
    
    /// Setup identification criteria
    const std::vector<std::string> windVars = {"u", "v"};
    const std::string pslVar = "psl";
    
    std::shared_ptr<IDCriterion> psl_crit(new MinCriterion(pslVar, 990.0));
    std::shared_ptr<IDCriterion> wind_crit(new MaxMagnitudeCriterion(windVars[0], windVars[1], 22.0));
    std::vector<std::shared_ptr<IDCriterion>> criteria = {psl_crit, wind_crit};

    /// Create sample data
    const DateTime coding_day(2019, 5, 12, 12);
    const Index tind = 20;
    const std::string fname("fake_file.nc");
    const Int npts = 10;
    std::vector<Real> sample_lats(npts);
    std::vector<Real> sample_lons(npts);
    
    std::vector<typename UnstructuredLayout::horiz_index_type> unst_inds(npts);
    std::vector<typename LatLonLayout::horiz_index_type> ll_inds(npts);
    
    std::vector<Real> sample_psl(npts);
    std::vector<Real> sample_u(npts);
    std::vector<Real> sample_v(npts);
    for (Int i=0; i<npts; ++i) {
        sample_lats[i] = i-5.0;
        sample_lons[i] = 175.0 + i;
        unst_inds[i][0] = 450+i;
        ll_inds[i][0] = 90+i;
        ll_inds[i][1] = 175+i;
        sample_u[i] = 20.0 + (i<5 ? i : -i);
        sample_v[i] = 5.0 + (i<5 ? 0.5*i : -0.5*i);
        sample_psl[i] = 990.0 + (i%2==0 ? -i : 2*i);
    }
    
    /// Create sample Sectors
    Sector<UnstructuredLayout> usec(0, 180, dummy_radius, sample_lats, sample_lons, unst_inds, criteria.size(), 0);
    usec.allocWorkspaces(criteria);
    usec.workspaces[0].fillData(pslVar, sample_psl);
    usec.workspaces[1].fillData(windVars[0], sample_u);
    usec.workspaces[1].fillData(windVars[1], sample_v);
    std::cout << "--- unst. sector ---" << std::endl;
    std::cout << usec.infoString();
    usec.linkToData();
    
    Sector<LatLonLayout> llsec(0, 180, dummy_radius, sample_lats, sample_lons, ll_inds, criteria.size(), 0);
    llsec.allocWorkspaces(criteria);
    llsec.workspaces[0].fillData(pslVar, sample_psl);
    llsec.workspaces[1].fillData(windVars[0], sample_u);
    llsec.workspaces[1].fillData(windVars[1], sample_v);
    std::cout << "--- latlon sector ---" << std::endl;
    std::cout << llsec.infoString();
    llsec.linkToData();
    
    std::vector<std::shared_ptr<Event<UnstructuredLayout>>> unst_events = usec.evaluateCriteriaAtTimestep(criteria,
        coding_day, fname, tind);
        
    std::vector<std::shared_ptr<Event<LatLonLayout>>> ll_events = llsec.evaluateCriteriaAtTimestep(criteria, 
        coding_day, fname, tind);
        
    std::cout << "--- unst. events ---" << std::endl;
    for (int i=0; i<unst_events.size(); ++i) {
        std::cout << unst_events[i]->infoString();
    }

    std::cout << "--- latlon events ---" << std::endl;
    for (int i=0; i<ll_events.size(); ++i) {
        std::cout << ll_events[i]->infoString();
    }
    
std::cout << "tests pass." << std::endl;
return 0;
}
