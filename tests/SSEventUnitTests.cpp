#include "SSEvent.hpp"
#include "SSEventTraits.hpp"
#include "SSDateTime.hpp"
#include "SSUtilities.hpp"
#include "SSDefs.hpp"
#include <memory>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
std::cout << "testing Event class..." << std::endl;

    const Int time_step_size = 6; // hours
    const DateTime dt1(2017, 1, 18, 0);
    const DateTime dt2(2017, 1, 18, time_step_size);
    
    const Real lat1 = 45.0;
    const Real lon1 = 0;
    const Real lat2 = 45.2;
    const Real lon2 = 0.2;
    
    const typename UnstructuredLayout::horiz_index_type unst_ind1({1});
    const typename UnstructuredLayout::horiz_index_type unst_ind2({2});
    const typename LatLonLayout::horiz_index_type ll_ind1({1,1});
    const typename LatLonLayout::horiz_index_type ll_ind2({1,2});
    
    const Real psl1 = 990.0;
    const Real psl2 = 992.0;
    const Real vort1 = 1.0e-4;
    const Int time_ind = 1;
    const std::string fname("fake_file.nc");
    const Real radius = 500.0;
    
    {// unstructured layout tests
    std::cout << "Event<UnstructuredLayout> tests" << std::endl;
    std::shared_ptr<Event<UnstructuredLayout>> 
        evpsl1(new Event<UnstructuredLayout>("min(PSL)", psl1, lat1, lon1, dt1, unst_ind1, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
    std::shared_ptr<Event<UnstructuredLayout>>
        evpsl2(new Event<UnstructuredLayout>("min(PSL)", psl2, lat2, lon2, dt1, unst_ind2, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
    
    std::shared_ptr<Event<UnstructuredLayout>>
        evvort1(new Event<UnstructuredLayout>("max(VOR)", vort1, lat2, lon2, dt1, unst_ind2, time_ind, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
    
    std::cout << evpsl1->infoString();
    std::cout << "evpsl1 is duplicate of evpsl2 (false) : " << 
            (evpsl1->isDuplicate(*evpsl2) ? "true" : "false") << std::endl;
    if (evpsl1->isDuplicate(*evpsl2)) 
        throw std::runtime_error("impossible!");
    std::cout << "evpsl2 is near evpsl1 (true) : " << 
            (evpsl2->isNear(*evpsl1, radius) ? "true" : "false") << std::endl;
    if (!evpsl2->isNear(*evpsl1, radius)) 
        throw std::runtime_error("impossible!");
    std::cout << "evpsl2 is less intense than evpsl1 (true) : " 
            << (evpsl2->lowerIntensity(*evpsl1) ? "true" : "false") << std::endl;
    if (!evpsl2->lowerIntensity(*evpsl1)) 
        throw std::runtime_error("impossible!");
    std::cout << "evpsl1 is redundant with evpsl2 (true) : " << 
        (evpsl1->isRedundant(*evpsl2, radius) ? "true" : "false") << std::endl;
    if (!evpsl1->isRedundant(*evpsl2, radius)) 
        throw std::runtime_error("impossible!");
        
    evpsl1->addRelated(evvort1);
    std::cout << evpsl1->infoString();
    }
    {// latlon layout tests
    std::cout << "Event<LatLonLayout> tests: " << std::endl;
    std::shared_ptr<Event<LatLonLayout>> 
        evpsl1(new Event<LatLonLayout>("min(PSL)", psl1, lat1, lon1, dt1, ll_ind1, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
    std::shared_ptr<Event<LatLonLayout>>
        evpsl2(new Event<LatLonLayout>("min(PSL)", psl2, lat2, lon2, dt1, ll_ind2, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
    
    std::shared_ptr<Event<LatLonLayout>>
        evvort1(new Event<LatLonLayout>("max(VOR)", vort1, lat2, lon2, dt1, ll_ind2, time_ind, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
    
    std::cout << evpsl1->infoString();
    std::cout << "evpsl1 is duplicate of evpsl2 (false) : " << 
        (evpsl1->isDuplicate(*evpsl2) ? "true" : "false") << std::endl;
    if (evpsl1->isDuplicate(*evpsl2)) throw std::runtime_error("impossible!");
    std::cout << "evpsl2 is near evpsl1 (true) : " << (evpsl2->isNear(*evpsl1, radius) ? "true" : "false") << std::endl;
    if (!evpsl2->isNear(*evpsl1, radius)) throw std::runtime_error("impossible!");
    std::cout << "evpsl2 is less intense than evpsl1 (true) : " 
        << (evpsl2->lowerIntensity(*evpsl1) ? "true" : "false") << std::endl;
    if (!evpsl2->lowerIntensity(*evpsl1)) throw std::runtime_error("impossible!");
    std::cout << "evpsl1 is redundant with evpsl2 (true) : " << 
        (evpsl1->isRedundant(*evpsl2, radius) ? "true" : "false") << std::endl;
    if (!evpsl1->isRedundant(*evpsl2, radius)) throw std::runtime_error("impossible!");
        
    evpsl1->addRelated(evvort1);
    std::cout << evpsl1->infoString();
    
    }
    
    

std::cout << "tests pass." << std::endl;
return 0;
}
