#include "SSEvent.hpp"
#include "SSIdCriteria.hpp"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <exception>

namespace StrideSearch {

template <typename DataLayout>
std::string Event<DataLayout>::infoString(int tab_level) const {
    std::ostringstream ss;
    std::string tabstr("");
    for (int i=0; i<tab_level; ++i) {
        tabstr += "\t";
    }
    ss << tabstr << "Event record:" << std::endl;
    ss << tabstr << "\tdescription = " << desc << std::endl;
    ss << tabstr << "\tvalue = " << value << std::endl;
    ss << tabstr << "\t(lat,lon) = (" << lat << "," << lon << ")" << std::endl;
    ss << tabstr << "\tloc_ind = " << loc_ind << std::endl;
    ss << tabstr << "\tloc_ind_3d = " << loc_ind_3d << std::endl;
    ss << tabstr << "\ttime_ind = " << time_ind << std::endl;
    ss << tabstr << "\tdatetime = " << datetime.isoFullStr() << std::endl;
    ss << tabstr << "\tfilename = " << filename << std::endl;
    ss << tabstr << "\tisReferenced = " << (isReferenced ? "true" : "false") << std::endl;
    if (relatedEvents.size() > 0) {
        ss << tabstr << "\tRelated Events:" << std::endl;
        ss << tabstr << "\t>>>>>>>>>>>>>>>>>>>\n";
        for (int i = 0; i < relatedEvents.size(); ++i)
            ss << relatedEvents[i]->infoString(tab_level+1);
    }
    ss << tabstr << "--------------------\n";
    return ss.str();
}

template <typename DataLayout>
std::string Event<DataLayout>::mgrCSVString() const {
    //datetime,desc,value,lat,lon,loc_ind,loc_ind_3d,time_ind,filename
    std::ostringstream ss;
    ss << datetime.DTGString() << ',' << desc << ',' << value << ',' << lat << ',' << lon << ','
       << loc_ind << ',' << loc_ind_3d << ',' << time_ind << ',' << filename << '\n';
    return ss.str();
}

template <typename DataLayout>
void Event<DataLayout>::addRelated(std::shared_ptr<Event> relEv) {
    if (datetime == relEv->datetime && time_ind == relEv->time_ind) {
        relatedEvents.push_back(relEv);
        relEv->isReferenced = true;
    }
    else {
        throw std::runtime_error("Related Events must have the same DateTime and time_ind.");
    }
}

template <typename DataLayout>
bool Event<DataLayout>::lowerIntensity(const Event& other) const {
    bool result = false;
    if (desc == other.desc) {
        switch (intensity_comparison) {
            case (GREATER_THAN) : {
                result = value < other.value;
                break;
            }
            case (LESS_THAN) : {
                result = value > other.value;
                break;
            }
        }
    }
    else {
        throw std::runtime_error("Cannot compare Events of different type.");
    }
    return result;
}

template <typename DataLayout>
bool Event<DataLayout>::isNear(const Event& other, const Real dist) const {
    return sphereDistance(lat,lon, other.lat, other.lon) <= dist;
}

template <typename DataLayout>
bool Event<DataLayout>::isRedundant(const Event& other, const Real dist) const {
    return desc==other.desc && datetime == other.datetime && isNear(other, dist);
}

template <typename DataLayout>
Real Event<DataLayout>::minRelatedDistance() const {
    Real result = 0;
    if (relatedEvents.size() > 0) {
        result = 2*PI*EARTH_RADIUS_KM;
        for (int i=0; i<relatedEvents.size(); ++i) {
            const Real dist = sphereDistance(lat,lon, relatedEvents[i]->lat, relatedEvents[i]->lon);
            if (dist < result) {
                result = dist;
            }
        }
    }
    return result;
}

template <typename DataLayout>
Real Event<DataLayout>::maxRelatedDistance() const {
    Real result = 0;
    for (int i=0; i<relatedEvents.size(); ++i) {
        const Real dist = sphereDistance(lat,lon, relatedEvents[i]->lat, relatedEvents[i]->lon);
        if (dist > result){
            result = dist;
        }
    }
    return result;
}

template <typename DataLayout>
Real Event<DataLayout>::distance(const Event& other) const {
    return sphereDistance(lat,lon, other.lat, other.lon);
}

template <typename DataLayout>
std::set<std::string> Event<DataLayout>::getDescriptions() const {
    std::set<std::string> result;
    result.insert(desc);
    for (int i=0; i<relatedEvents.size(); ++i) {    
        result.insert(relatedEvents[i]->desc);
    }
    return result;
}

template <typename DataLayout>
bool Event<DataLayout>::isCollocated(const IDCriterion* crit1, const IDCriterion* crit2, const Real distThreshold) const {
    bool result = false;
    const std::set<std::string> descs = getDescriptions();
    if (descs.count(crit1->description()) == 1 && descs.count(crit2->description()) == 1) {
        // If both criteria have corresponding Events, get their locations
        Real lat1, lon1;
        Real lat2, lon2;
        if (desc == crit1->description()) {
            lat1 = lat;
            lon1 = lon;
            for (int i=0; i<relatedEvents.size(); ++i) {
                if (relatedEvents[i]->desc == crit2->description()) {
                    lat2 = relatedEvents[i]->lat;
                    lon2 = relatedEvents[i]->lon;
                }
            }
        }
        else if (desc == crit2->description()) {
            lat2 = lat;
            lon2 = lon;
            for (int i=0; i<relatedEvents.size(); ++i) {
                if (relatedEvents[i]->desc == crit1->description()) {
                    lat1 = relatedEvents[i]->lat;
                    lon1 = relatedEvents[i]->lon;
                }
            }
        }
        else {
            for (int i=0; i<relatedEvents.size(); ++i) {
                if (relatedEvents[i]->desc == crit1->description()) {
                    lat1 = relatedEvents[i]->lat;
                    lon1 = relatedEvents[i]->lon;
                }
                if (relatedEvents[i]->desc == crit2->description()) {
                    lat2 = relatedEvents[i]->lat;
                    lon2 = relatedEvents[i]->lon;
                }
            }
        }
        // compare their distance against the threshold
        result = sphereDistance(lat1,lon1, lat2,lon2) <= distThreshold;
    }
    return result;
}

template <typename DataLayout>
std::vector<std::shared_ptr<Event<DataLayout>>> Event<DataLayout>::flatten() const {
    std::shared_ptr<Event<DataLayout>> main(new Event<DataLayout>(this->desc, this->value,
        this->lat, this->lon, this->datetime, this->loc_ind, this->time_ind, this->filename,
        this->intensity_comparison, this->spatial_dependence));
    main->loc_ind_3d = this->loc_ind_3d;
    std::vector<std::shared_ptr<Event<DataLayout>>> result;
    result.push_back(main);
    for (int i=0; i<relatedEvents.size(); ++i) {
        result.push_back(relatedEvents[i]);
    }
    return result;
}

/// ETI
template class Event<UnstructuredLayout>;
template class Event<LatLonLayout>;

}
