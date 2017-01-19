#ifndef _STRIDE_SEARCH_EVENT_H_
#define _STRIDE_SEARCH_EVENT_H_

#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"
#include <string>
#include <iostream>

typedef std::pair<double, double> ll_coord_type;
typedef std::vector<int> indices_type;



class Event {
    public:
        enum EventType {Max, Min};
        
        Event(const std::string dsc, const double value, const ll_coord_type ll, const DateTime dt, 
            const indices_type ind, const std::string fname, const int tind, const EventType tp);
        virtual ~Event(){};
        
        std::string infoString(int tabLevel = 0) const;
        
        inline void addRelated(Event* relEv){
            relatedEvents.push_back(relEv);
            relEv->isReferenced = true;
        }
          
        bool lowerIntensity(const Event& other) const { 
            if (this->desc == other.desc) {
                if (this->type == Max)
                    return this->val < other.val;
                else 
                    return this->val > other.val;
            }
            else {
                std::cerr << "Event operator < ERROR: can only compare events of same kind.\n";
            }
        }
        
        inline bool isDuplicate(const Event& other) const {
            return desc == other.desc && datetime == other.datetime && dataIndex == other.dataIndex;
        }
        
        bool isNear(const Event& other, const double distThreshold) const;
        bool isRedundant(const Event& other, const double distThreshold) const;
        
                
    protected:
        std::string desc;
        double val;
        ll_coord_type latLon;
        DateTime datetime;
        indices_type dataIndex;
        std::string filename;
        int time_index;
        std::vector<Event*> relatedEvents;
        bool isReferenced;
        EventType type;
};
inline bool operator < (const Event& left, const Event& right) {return left.lowerIntensity(right); }
inline bool operator > (const Event& left, const Event& right) {return right < left; }


#endif
