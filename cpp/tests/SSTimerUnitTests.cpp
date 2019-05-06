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
    timeval t0 = time::tic();
    index_type counter = 0;
    scalar_type arr[50];
    for (index_type i = 0; i < 10000000; ++i) {
        counter = 50 * i + 2;
        arr[i%50] = counter * 0.5;
    }
    index_type ct = counter;
    testTimer.end();
    std::cout << "tic-toc: " << time::toc(t0) << std::endl;
    std::cout << testTimer.infoString();
return 0;
}
