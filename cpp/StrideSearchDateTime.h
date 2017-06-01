#ifndef _STRIDE_SEARCH_DATE_TIME_H_
#define _STRIDE_SEARCH_DATE_TIME_H_

#include <string>

namespace StrideSearch {

struct DateTime {
    int year;
    int month;
    int day;
    int hour;
    
    DateTime(const int yr = 1850, const int mo = 1, const int dy = 1, const int hr = 0);
       
    std::string DTGString(std::string time_zone_string = "Z") const;
    std::string intString() const;
};
inline bool operator==(const DateTime& left, const DateTime& right){
    return left.year == right.year && left.month == right.month && left.day == right.day && left.hour == right.hour;
}

}
#endif
