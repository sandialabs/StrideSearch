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
    
    std::string moStr;
    switch (month) {
        case 1:
            moStr = "JAN";
            break;
        case 2:
            moStr = "FEB";
            break;
        case 3:
            moStr = "MAR";
            break;
        case 4:
            moStr = "APR";
            break;
        case 5:
            moStr = "MAY";
            break;
        case 6:
            moStr = "JUN";
            break;
        case 7:
            moStr = "JUL";
            break;
        case 8:
            moStr = "AUG";
            break;
        case 9:
            moStr = "SEP";
            break;
        case 10:
            moStr = "OCT";
            break;
        case 11:
            moStr = "NOV";
            break;
        case 12:
            moStr = "DEC";
            break;
    }
    return dyStr + hrStr + time_zone_string + moStr + yrStr;
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