#include "StrideSearchUtilities.h"
#include "StrideSearchEvent.h"
#include <iostream>
#include <sstream>
#include <iomanip>

Event::Event(const std::string dsc, const double value, const ll_coord_type ll, const DateTime dt, 
            const indices_type ind, const std::string fname, const int tind, const Event::EventType tp) {
    desc = dsc;
    val = value;
    latLon = ll;
    datetime = dt;
    dataIndex = ind;
    filename = fname;
    time_index = tind;
    type = tp;
    isReferenced = false;           
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

