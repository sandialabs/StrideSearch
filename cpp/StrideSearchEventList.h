#ifndef _STRIDE_SEARCH_EVENT_LIST_
#define _STRIDE_SEARCH_EVENT_LIST_

#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchEvent.h"
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include <memory>

namespace StrideSearch {

class EventList {
    public:
        EventList();
        EventList(const std::vector<std::shared_ptr<Event>>& eventVec);
    
        void extend(const EventList& other);
        
        inline index_type size() const {return events.size();}
    
        scalar_type minSeparationDistance() const;
        scalar_type maxSeparationDistance() const;
        
        std::string infoString(const int tablevel = 0, const bool printAll = false) const;
        
        void removeDuplicates(const scalar_type distThreshold);
        
        std::map<DateTime, std::vector<Event>> separateByDate() const;
        
        void consolidateRelatedEvents(const scalar_type distThreshold);
    
    protected:
        std::vector<std::shared_ptr<Event>> events;
};


}
#endif
