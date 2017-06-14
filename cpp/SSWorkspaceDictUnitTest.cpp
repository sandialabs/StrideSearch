#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchWorkspaceDict.h"
#include <vector>
#include <string>
#include <iostream>

using namespace StrideSearch;

int main() {
    std::vector<std::string> varnames = {"U850", "V850"};
    
    const int nDataInds = 10;
    
    WorkspaceDict wspc(varnames, nDataInds);
    
    wspc.dict.at("U850")[2] = 10.0;
    wspc.dict.at("U850")[3] = 30.0;
    
    const std::vector<scalar_type>& v850ref = wspc.dict.at("V850");
    const std::vector<scalar_type>& u850ref = wspc.getConstDataReference("U850");

    std::cout << wspc << std::endl;

    std::cout << "&wspc.dict.at('V850') = " << &wspc.dict.at("V850") << std::endl;
    std::cout << "&v850ref = " << &v850ref << std::endl;
    std::cout << "v850ref.size() = " << v850ref.size() << std::endl;
    std::cout << "u850ref[3] = " << u850ref[3] << std::endl;

    std::cout << "TEST PASSED." << std::endl;
return 0;
}

