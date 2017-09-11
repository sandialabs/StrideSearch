#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchTimer.h"
#include <chrono>
#include <iostream>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();

    Timer testTimer("unitTestTimer");
    testTimer.start();
    
    index_type counter = 0;
    for (index_type i = 0; i < 10000000; ++i) {
        counter = 50 * i + 2;
    }
    
    testTimer.end();
    std::cout << testTimer.infoString();
return 0;
}
