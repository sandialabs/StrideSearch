#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSInput.hpp"
#include <iostream>
#include <string>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
std::cout << "Testing Input struct...\n";

    Input input(argc, argv);
    
    std::cout << input.help_msg();


std::cout << "tests pass.\n";
}