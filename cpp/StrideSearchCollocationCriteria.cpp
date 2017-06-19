#include "StrideSearchCollocationCriteria.h"
#include <set>
#include <memory>

namespace StrideSearch {

CollocationCriterion::CollocationCriterion(const std::string& desc1, const std::string& desc2, 
    const scalar_type thresh) : eventDesc1(desc1), eventDesc2(desc2), distanceThreshold(thresh) {};

bool CollocationCriterion::evaluate(const Event& event) const {
    bool result = false;
    std::set<std::string> descSet;
    descSet.insert(event.desc);
    const Event* evA;
    const Event* evB;
    for (index_type i = 0; i < event.relatedEvents.size(); ++i) {
        descSet.insert(event.relatedEvents[i]->desc);
    }
    if (descSet.count(eventDesc1) + descSet.count(eventDesc2) == 2) {
        if (event.desc == eventDesc1) {
            evA = &event;
            for (index_type i = 0; i < event.relatedEvents.size(); ++i) {
                if (event.relatedEvents[i]->desc == eventDesc2)
                    evB = event.relatedEvents[i].get();
            }
        }
        else if (event.desc == eventDesc2) {
            evA = &event;
            for (index_type i = 0; i < event.relatedEvents.size(); ++i) {
                if (event.relatedEvents[i]->desc == eventDesc1)
                    evB = event.relatedEvents[i].get();
            }
        }
        else {
            for (index_type i = 0; i < event.relatedEvents.size(); ++i) {
                if (event.relatedEvents[i]->desc == eventDesc1)
                    evA = event.relatedEvents[i].get();
                if (event.relatedEvents[i]->desc == eventDesc2)
                    evB = event.relatedEvents[i].get();
            }
        }
        if (evA->isNear(*evB, distanceThreshold))
            result = true;
    }
    return result;
}

}
