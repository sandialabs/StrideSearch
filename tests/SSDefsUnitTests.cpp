#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include <typeinfo>
#include <iostream>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    
    std::cout << "Unstructured: " << typeid(UnstructuredLayout).name() << " has data_index_type = " << typeid(UnstructuredLayout::data_index_type).name() << std::endl;
    
    std::cout << "Uniform: " << typeid(LatLonLayout).name() << " has data_index_type = " << typeid(LatLonLayout::data_index_type).name() << std::endl;


std::cout << "tests pass." << std::endl;    
return 0;
}