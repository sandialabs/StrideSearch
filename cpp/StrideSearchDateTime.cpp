#include "StrideSearchDateTime.h"
#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>

namespace StrideSearch {

DateTime::DateTime(const int yr, const int mo, const int dy, const int hr) {
    year = yr;
    month = mo;
    day = dy;
    hour = hr;
    if (monthDayMap.empty()) 
        buildMonthDayMap();
}

void DateTime::buildMonthDayMap() {
    monthDayMap.emplace(1, 31);
    monthDayMap.emplace(2, 28);
    monthDayMap.emplace(3, 31);
    monthDayMap.emplace(4, 30);
    monthDayMap.emplace(5, 31);
    monthDayMap.emplace(6, 30);
    monthDayMap.emplace(7, 31);
    monthDayMap.emplace(8, 31);
    monthDayMap.emplace(9, 30);
    monthDayMap.emplace(10, 31);
    monthDayMap.emplace(11, 30);
    monthDayMap.emplace(12, 31);
    
    monthStringMap.emplace(1, "JAN");
    monthStringMap.emplace(2, "FEB");
    monthStringMap.emplace(3, "MAR");
    monthStringMap.emplace(4, "APR");
    monthStringMap.emplace(5, "MAY");
    monthStringMap.emplace(6, "JUN");
    monthStringMap.emplace(7, "JUL");
    monthStringMap.emplace(8, "AUG");
    monthStringMap.emplace(9, "SEP");
    monthStringMap.emplace(10, "OCT");
    monthStringMap.emplace(11, "NOV");
    monthStringMap.emplace(12, "DEC");
}

std::string DateTime::DTGString(std::string time_zone_string) const {
    std::ostringstream ss4;
    ss4 << std::setw(4) << std::setfill('0') << year;
    std::string yrStr(ss4.str());
    
    std::ostringstream ss2;
    ss2 << std::setw(2) << std::setfill('0') << day;
    std::string dyStr(ss2.str());
    
    ss2.str(std::string());
    ss2 << std::setw(2) << std::setfill('0') << hour << "00";
    std::string hrStr(ss2.str());
    
    std::string moStr = monthString(month);

    return dyStr + hrStr + time_zone_string + moStr + yrStr;
}

std::string DateTime::monthString() const {
    return monthStringMap.at(month);
}

std::string DateTime::monthString(const int mInt) const {
    return monthStringMap.at(mInt);
}


bool operator < (const DateTime& left, const DateTime& right) {
    bool result = false;
    if (left.year < right.year)
        result = true;
    else if (left.year == right.year) {
        if (left.month < right.month) {
            result = true;
        }
        else if (left.month == right.month) {
            if (left.day < right.day) {
                result = true;
            }
            else if (left.day == right.day) {
                if (left.hour < right.hour)
                    result = true;
            }
        }
    }
    return result;
}

bool operator > (const DateTime& left, const DateTime& right) {
    return right < left;
}

std::string DateTime::intString() const{
    std::ostringstream ss;
    ss << std::setw(4) << std::setfill('0') << year;
    ss << std::setw(2) << std::setfill('0') << month;
    ss << std::setw(2) << std::setfill('0') << day;
    ss << std::setw(2) << std::setfill('0') << hour << "00";
    return ss.str();
}

}