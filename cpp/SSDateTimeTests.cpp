#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDateTime.h"

#include <iostream>

using namespace StrideSearch;

int main (int argc, char* argv[]) {

    print_copyright();
    
    int year = 2017;
    int month = 1;
    int day = 18;
    int hour = 9;
    DateTime codingDay(year, month, day, hour);
    std::cout << "DateTime::DTGString() = " << codingDay.DTGString("T") << std::endl;
    std::cout << "DateTime::intString() = " << codingDay.intString() << std::endl;
    
    DateTime codingDay2(year, month, day, hour);
    std::cout << "(True) Equivalent date-times? " << (codingDay == codingDay2 ? "true" : "false") << std::endl;
    
    codingDay2 = DateTime(year, month, day, hour + 1);
    std::cout << "(False) Equivalent date-times? " << (codingDay == codingDay2 ? "true" : "false") << std::endl;
return 0;
}
