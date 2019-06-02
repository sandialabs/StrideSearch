#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSMPIManager.hpp"
#include <iostream>
#include <string>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
std::cout << "testing MPIManager class.\n";

    const Int nfiles = 51;
    const Int ns = 201;
    
    MPIManager serial_mgr(nfiles);
    
    MPIManager desktop_mgr(ns, 0, 4, MPIDistribute::TIMESTEPS);
    
    MPIManager big_mgr(nfiles, 0, 24);
    
    std::cout << "SERIAL:\n";
    std::cout << serial_mgr.infoString();
    
    std::cout << "DESKTOP:\n";
    std::cout << desktop_mgr.infoString();
    
    std::cout << "BIG:\n";
    std::cout << big_mgr.infoString();

std::cout << "tests pass.\n";
return 0;
}
