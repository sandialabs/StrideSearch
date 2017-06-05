#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchWorkspace.h"
#include <iostream>
#include <string>
#include <map>
#include <iomanip>

using namespace StrideSearch;

int main (int argc, char* argv[]) {
    print_copyright();

    std::vector<std::string> varNames = {"vorticity", "surface pressure"};
    const int nVals = 8;
    Workspace1D wspc(varNames, nVals);
    wspc["surface pressure"][1] = 990.0;
    wspc["vorticity"][0] = 0.025;
    std::cout << wspc;
    
    Workspace2D sampleData2;
    std::cout << "declared sampleData2 = " << sampleData2;
    {
        Workspace2D sampleData(varNames, 5, 4);
        for (int i = 0; i < 5; ++i){
            for (int j = 0; j < 4; ++j){
                sampleData["vorticity"][i][j] = i + j;
                sampleData["surface pressure"][i][j] = i * j;
            }
        }
        std::cout << sampleData;
    
        sampleData2 = sampleData;
       
    }
    std::cout << "Copied Workspace2d: " << std::endl;
    std::cout << sampleData2;
return 0;
}