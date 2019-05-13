#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include "SSWorkspace.hpp"
#include <exception>
#include <typeinfo>

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
std::cout << "Workspace unit tests." << std::endl;

    std::vector<std::string> varnames = {"U850", "V850"};
    const Int nInds = 10;
    
    Workspace w1(varnames, nInds);
    w1.data.at("U850")[2] = 12.5;
    w1.data.at("U850")[3] = 30.0;
    
    const RealArray& cref = w1.getConstDataRef("U850");
    RealArray& ref = w1.getDataRef("V850");
    
    std::cout << w1 << std::endl;
    
    std::cout << "&w1.data.at('V850') = " << &w1.data.at("V850") << std::endl;
    std::cout << "&ref(V850) = " << &ref << std::endl;
    std::cout << "typid(&ref).name() = " << typeid(&ref).name() << std::endl;
    if (&ref != &w1.data.at("V850")) {
        throw std::runtime_error("something went wrong 1.");
    }
    
    std::cout << "&w1.data.at('U850') = " << &cref << std::endl;
    if (&cref != &w1.data.at("U850")) {
        throw std::runtime_error("something went wrong 2.");
    }
    std::cout << "cref[3] = " << cref[3] << std::endl;
    

std::cout << "tests pass." << std::endl;
return 0;
}
