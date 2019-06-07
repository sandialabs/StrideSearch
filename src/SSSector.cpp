#include "SSSector.hpp"
#include <sstream>
#include <cmath>

namespace StrideSearch {

template <typename DataLayout>
void Sector<DataLayout>::allocWorkspaces(const std::vector<std::shared_ptr<IDCriterion>>& criteria) {
    workspaces = std::vector<Workspace>(criteria.size());
    const Int sector_size = indices.size();
    for (Int i=0; i<criteria.size(); ++i) {
        workspaces[i] = Workspace(criteria[i]->varnames, sector_size);
    }
}

template <typename DataLayout>
void Sector<DataLayout>::allocWorkspace(const std::shared_ptr<IDCriterion> locator) {
    workspaces = std::vector<Workspace>(1);
    workspaces[0] = Workspace(locator->varnames, indices.size());
}

template <typename DataLayout>
std::string Sector<DataLayout>::infoString(const int tab_lev, const bool printWorkspaces) const {
    std::ostringstream ss;
    std::string tabstr("");
    for (int i=0; i<tab_lev; ++i) {
        tabstr += "\t";
    }
    ss << tabstr << "Sector Record:" << std::endl;
    ss << tabstr << "\tcenter (lat,lon) = (" << lat << "," << lon << ")" << std::endl;
    ss << tabstr << "\tradius = " << radius << std::endl;
    ss << tabstr << "\tworkspaces size = " << workspaces.size() << std::endl;
    ss << tabstr << "\tnDataPoints() = " << nDataPoints() << std::endl;
    ss << tabstr << "\tcontained coordinates: " << std::endl << tabstr << "\t";
    for (Int i=0; i<lats.size(); ++i) {
        ss << "(" << lats[i] << "," << lons[i] << ") ";
    }
    ss << std::endl << tabstr << "\thorizontal indices: " << std::endl << tabstr << "\t";
    for (Int i=0; i<indices.size(); ++i) {
        ss << indices[i] << " ";
    }
    ss << std::endl;
    if (printWorkspaces) {
        ss << tabstr << "\tworkspaces:" << std::endl;
        for (Int i=0; i<workspaces.size(); ++i) {
            ss << workspaces[i] << std::endl;
        }
    }
    ss << "---------------------------------------" << std::endl;
    return ss.str();
}

template <typename DataLayout>
std::vector<std::shared_ptr<Event<DataLayout>>> Sector<DataLayout>::evaluateCriteriaAtTimestep(
    std::vector<std::shared_ptr<IDCriterion>>& criteria, const DateTime& dt, 
    const std::string& fname, const Index time_ind) const {
    std::vector<std::shared_ptr<Event<DataLayout>>> result;
    for (Int i=0; i<criteria.size(); ++i) {
        if (criteria[i]->evaluate(workspaces[i])) {
            const Index wspc_ind = criteria[i]->wspcIndex;
            Real elat, elon;
            if (criteria[i]->useSectorLocation()) {
                elat = lat;
                elon = lon;
            }
            else {
                elat = lats[wspc_ind];
                elon = lons[wspc_ind];
            }
            result.push_back(std::shared_ptr<Event<DataLayout>>(
                new Event<DataLayout>(criteria[i]->description(), criteria[i]->val,
                    elat, elon, dt, indices[wspc_ind], time_ind, fname,
                    criteria[i]->compareKind, criteria[i]->locationKind)));
        }
    }
    return result;
}

template <typename DataLayout>
std::shared_ptr<Event<DataLayout>> Sector<DataLayout>::evaluateLocatorCriterionAtTimestep(
    std::shared_ptr<IDCriterion> locator, const DateTime& dt, const std::string& fname, const Index time_ind) const {
    std::shared_ptr<Event<DataLayout>> result;
    if (locator->evaluate(workspaces[0])) {
        const Index wspc_ind = locator->wspcIndex;
        result = std::shared_ptr<Event<DataLayout>>(new Event<DataLayout>(locator->description(), locator->val,
            lats[wspc_ind], lons[wspc_ind], dt, indices[wspc_ind], time_ind, fname, locator->compareKind, INDEPENDENT));
    }
    return result;
}


/// Link Sector To Data implementation for UnstructuredLayout
template <> template <>
void Sector<UnstructuredLayout>::linkHelper<UnstructuredLayout>(const KDTree& tree, 
    const std::shared_ptr<NCReader> ncr) {
    const Real avgCellSizeSq = ncr->avgResKm * ncr->avgResKm;
    const Int n_estimate = std::ceil(4*PI*radius*radius/(avgCellSizeSq));
    auto searchReturn = tree.search(lat, lon, radius, n_estimate);
    for (Int i=0; i<searchReturn.size(); ++i) {
        const Index tree_ind = searchReturn[i].first;
        indices.push_back(dynamic_cast<UnstructuredNCReader*>(ncr.get())->getDataIndexFromTreeIndex(tree_ind));
        lats.push_back(ncr->getLat(tree_ind));
        lons.push_back(ncr->getLon(tree_ind));
    }
}

/// Link Sector To Data implementation for LatLonLayout
template <> template <>
void Sector<LatLonLayout>::linkHelper<LatLonLayout>(const KDTree& tree, const std::shared_ptr<NCReader> ncr) {
    const Real avgCellSizeSq = ncr->avgResKm * ncr->avgResKm;
    const Int n_estimate = std::ceil(4*PI*radius*radius/avgCellSizeSq);
    auto searchReturn = tree.search(lat, lon, radius, n_estimate);
    for (Int i=0; i<searchReturn.size(); ++i) {
        const Index tree_ind = searchReturn[i].first;
        indices.push_back(dynamic_cast<LatLonNCReader*>(ncr.get())->getDataIndexFromTreeIndex(tree_ind));
        lats.push_back(ncr->getLat(tree_ind));
        lons.push_back(ncr->getLon(tree_ind));
    }
}        


/// ETI
template struct Sector<UnstructuredLayout>;
template struct Sector<LatLonLayout>;
}
