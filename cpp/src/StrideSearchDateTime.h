#ifndef _STRIDE_SEARCH_DATE_TIME_H_
#define _STRIDE_SEARCH_DATE_TIME_H_

#include <string>
#include <map>

namespace StrideSearch {

typedef std::map<int, int> calendar_map_type;
typedef std::map<int, std::string> month_string_map_type;

static calendar_map_type monthDayMap;
static month_string_map_type monthStringMap;

/// Bare-bones date-time structure and methods.
class DateTime {
    public:
        int year;
        int month;
        int day;
        int hour;
        
        /// Constructor.
        DateTime(const int yr = 1850, const int mo = 1, const int dy = 1, const int hr = 0);
    
        /// Return a formatted Date-Time-Group string.   
        std::string DTGString(std::string time_zone_string = "Z") const;
    
        /// Return a string formatted YYYYMMDDHH00.
        std::string intString() const;
    
        std::string monthString(const int mInt) const;
        std::string monthString() const;
        
        
    protected:
        void buildMonthDayMap();
        
};
inline bool operator==(const DateTime& left, const DateTime& right){
    return left.year == right.year && left.month == right.month && left.day == right.day && left.hour == right.hour;
}

bool operator < (const DateTime& left, const DateTime& right); 

bool operator > (const DateTime& left, const DateTime& right); 

}
#endif
