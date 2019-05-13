#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include "SSEventSet.hpp"
#include <iostream>
#include <exception>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
std::cout << "testing EventSet class." << std::endl;
    /// Make up sample data
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
    
    const Real psl_1 = 990.0;
    const Real psl_2 = 992.0;
    const Real vort_1 = 1.0e-4;
    const Int time_ind = 1;
    const std::string fname("fake_file.nc");
    const Real radius = 500.0;

    {
        typedef UnstructuredLayout Layout;
        typedef Event<Layout> event_type;
        typedef std::shared_ptr<event_type> event_ptr;
        
        event_ptr psl1(new event_type("min(psl)", psl_1, lat1, lon1, dt1, unst_ind1, time_ind, fname, 
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
        event_ptr psl2(new event_type("min(psl)", psl_2, lat2, lon2, dt1, unst_ind2, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
        event_ptr vort1(new event_type("max(vort)", vort_1, lat2, lon2, dt1, unst_ind2, time_ind, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
        event_ptr vort2(new event_type("max(vort)", vort_1, lat2, lon2, dt2, unst_ind2, time_ind+1, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
        
        std::vector<event_ptr> evec = {psl1, psl2, vort1, vort2};
        
        const Int start_size = evec.size();
        EventSet<Layout> eset(evec);
        std::cout << eset.infoString(0, true);
        
        std::cout << "*** removing duplicates ***" << std::endl;
        eset.removeDuplicates(radius);
        std::cout << eset.infoString(0, true);
        if (eset.size() != start_size-1) 
            throw std::runtime_error("impossible!");
        
        std::cout << "*** consolidating ***" << std::endl;
        eset.consolidateRelated(radius);
        std::cout << eset.infoString(0,true);
        if (eset.size() != start_size-2) 
            throw std::runtime_error("impossible!");
        
        std::cout << "*** separating by DateTime ***" << std::endl;
        std::map<DateTime, std::vector<event_type>> esplit = eset.separateByDateTime();
        for (auto& e : esplit) {
            std::cout << e.first.DTGString() << ":" << std::endl;
            for (int i=0; i<e.second.size(); ++i)
                std::cout << e.second[i].infoString(1);
        }
        
    }
    {
        typedef LatLonLayout Layout;
        typedef Event<Layout> event_type;
        typedef std::shared_ptr<event_type> event_ptr;
        
        event_ptr psl1(new event_type("min(psl)", psl_1, lat1, lon1, dt1, ll_ind1, time_ind, fname, 
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
        event_ptr psl2(new event_type("min(psl)", psl_2, lat2, lon2, dt1, ll_ind2, time_ind, fname,
            IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT));
        event_ptr vort1(new event_type("max(vort)", vort_1, lat2, lon2, dt1, ll_ind2, time_ind, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
        event_ptr vort2(new event_type("max(vort)", vort_1, lat2, lon2, dt2, ll_ind2, time_ind+1, fname,
            IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT));
        
        std::vector<event_ptr> evec = {psl1, psl2, vort1, vort2};
        
        EventSet<Layout> eset(evec);
        std::cout << eset.infoString(0, true);
        
        std::cout << "*** removing duplicates ***" << std::endl;
        eset.removeDuplicates(radius);
        std::cout << eset.infoString(0, true);
        
        std::cout << "*** consolidating ***" << std::endl;
        eset.consolidateRelated(radius);
        std::cout << eset.infoString(0,true);
        
        std::cout << "*** separating by DateTime ***" << std::endl;
        std::map<DateTime, std::vector<event_type>> esplit = eset.separateByDateTime();
        for (auto& e : esplit) {
            std::cout << e.first.DTGString() << ":" << std::endl;
            for (int i=0; i<e.second.size(); ++i)
                std::cout << e.second[i].infoString(1);
        }
        
    }
std::cout << "tests pass." << std::endl;
return 0;
}
