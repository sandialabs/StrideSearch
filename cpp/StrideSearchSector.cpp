#include "StrideSearchSector.h"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>

namespace StrideSearch {

void Sector::defineWorkspace(const std::vector<IDCriterion*>& criteria){
    workspace = std::vector<WorkspaceDict>(criteria.size());
};

void Sector::allocWorkspace(const std::vector<IDCriterion*>& criteria) {
    for (int i = 0; i < criteria.size(); ++i) {
        workspace[i] = WorkspaceDict(criteria[i]->varnames, data_indices.size());
    }
}

std::vector<std::shared_ptr<Event>> Sector::evaluateCriteriaAtTimestep(std::vector<IDCriterion*>& criteria, const DateTime& dt, 
    const std::string& fname, const index_type timeIndex) {
    std::vector<std::shared_ptr<Event>> result;
    for (index_type i = 0; i < criteria.size(); ++i) {
        if (criteria[i]->evaluate(workspace[i])) {
            result.push_back(std::shared_ptr<Event>(new Event(criteria[i]->description(), criteria[i]->val, data_coords[criteria[i]->wspcIndex], 
                dt, data_indices[criteria[i]->wspcIndex], fname, timeIndex, criteria[i]->compareKind)));
        }
    }
    return result;
}
    
    
std::string Sector::infoString(const int tabLevel, const bool printWspc) const {
    std::string tabstr("");
    for (int i = 0; i < tabLevel; ++i)
        tabstr += "\t";
    std::ostringstream ss;
    ss << tabstr << "Sector Record: \n";
    ss << tabstr << "\tcenter (lat,lon) = (" << centerLat << ", " << centerLon << ")\n";
    ss << tabstr << "\tworkspace.size() = " << workspace.size() << std::endl;
    ss << tabstr << "\tdata_coords (lat,lon) :\n" << tabstr << "\t";
    for (index_type i = 0; i < data_coords.size(); ++i) {
        ss << "(" << data_coords[i].first << ", " << data_coords[i].second << ") ";
    }
    ss << std::endl << tabstr << "\tdata_indices : \n" << tabstr << "\t";
    for (index_type i = 0; i < data_indices.size(); ++i) {
        ss << "(";
        for (index_type j = 0; j < data_indices[i].size() - 1; ++j) {
            ss << data_indices[i][j] << ", ";
        }
        ss << data_indices[i][data_indices[i].size()-1] << ") " ;
    }
    if (printWspc) {
        ss << std::endl << "-- local data -- " << std::endl;
        for (int i = 0; i < workspace.size(); ++i)
            ss << workspace[i] << std::endl;
    }
    ss << std::endl << "--------------------------" << std::endl;
    return ss.str();
}

};