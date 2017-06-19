#include "StrideSearchUtilities.h"
#include "StrideSearchEvent.h"
#include <iostream>
#include <sstream>
#include <iomanip>

namespace StrideSearch {

Event::Event(const std::string dsc, const scalar_type value, const ll_coord_type ll, const DateTime dt, 
            const vec_indices_type& locInd, const std::string fname, const index_type tind, const Event::IntensityComparison tp) :
       desc(dsc), val(value), latLon(ll), datetime(dt), dataIndex(locInd), filename(fname), time_index(tind),
       compare(tp), isReferenced(false) {};

Event::Event() : desc("null"), val(0.0), latLon(0.0, 0.0), datetime(), dataIndex(std::vector<index_type>(1,-1)), 
    filename("nullfile"), time_index(-1), compare(Event::GREATER_THAN), isReferenced(false) {};

void Event::addRelated(std::shared_ptr<Event> relEv) {
    relatedEvents.push_back(relEv);
    relEv->isReferenced = true;
}

bool Event::lowerIntensity(const Event& other) const { 
    if (this->desc == other.desc) {
        if (this->compare == GREATER_THAN)
            return this->val < other.val;
        else 
            return this->val > other.val;
    }
    else {
        std::cerr << "Event operator < ERROR: can only compare events of same kind.\n";
        return false;
    }
}

scalar_type Event::minRelatedDistance() const {
    scalar_type result = 0.0;
    if (relatedEvents.size() > 0) {
        result = 2.0 * PI * EARTH_RADIUS_KM;
        for (index_type i = 0; i < relatedEvents.size(); ++i) {
            const scalar_type dist = sphereDistance(latLon, relatedEvents[i]->latLon);
            if (dist < result)
                result = dist;
        }
    }
    return result;
}

scalar_type Event::maxRelatedDistance() const {
    scalar_type result = 0.0;
    for (index_type i = 0; i < relatedEvents.size(); ++i) {
        const scalar_type dist = sphereDistance(latLon, relatedEvents[i]->latLon);
        if ( dist > result) 
            result = dist;
    }
    return result;
}



std::string Event::infoString(int tabLevel) const {
    std::string tabstr("");
    for (int i = 0; i < tabLevel; ++i)
        tabstr += "\t";
    std::ostringstream ss;
    ss << tabstr << "Event record:\n";
    ss << tabstr << "\tdescription: " << desc << std::endl;
    ss << tabstr << "\tvalue: " << val << std::endl;
    ss << tabstr << "\t(lat, lon) = (" << latLon.first << ", " << latLon.second << ")\n";
    ss << tabstr << "\tdata_index = (";
    for (int j = 0; j < dataIndex.size()-1; ++j)
        ss << dataIndex[j] << ", ";
    ss << dataIndex[dataIndex.size() - 1] << ")\n";
    ss << tabstr << "\tDTG: " << datetime.DTGString() << std::endl;
    ss << tabstr << "\tin file: " << filename << std::endl;
    ss << tabstr << "\tisReferenced: " << (isReferenced ? "true" : "false") << std::endl;
    if (relatedEvents.size() > 0) {
        tabLevel += 1;
        for (int i = 0; i < relatedEvents.size(); ++i)
            ss << relatedEvents[i]->infoString(tabLevel);
    }
    ss << tabstr << "--------------------\n";
    return ss.str();
}

bool Event::isNear(const Event& other, const double distThreshold) const {
    return (sphereDistance(this->latLon.first, this->latLon.second, 
            other.latLon.first, other.latLon.second) <= distThreshold);
}

bool Event::isRedundant(const Event& other, const double distThreshold) const {
    return this->desc == other.desc && this->datetime == other.datetime && this->isNear(other, distThreshold);
}

}
