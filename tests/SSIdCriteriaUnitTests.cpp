#include "StrideSearchConfig.h"
#include "SSWorkspace.hpp"
#include "SSIdCriteria.hpp"
#include <iostream>

using namespace StrideSearch;

void id_results(IDCriterion* idc, const Workspace& wspc);

int main(int argc, char* argv[]) {
print_copyright();

std::cout << "Testing identification criteria..." << std::endl;

//
//  create sample workspaces with dummy data
//
const int nPoints = 10;
const std::vector<std::string> vorticity_variables = {"vorticity", "latitude"};
const std::string slpname = "sea_level_pressure";
const std::vector<std::string> wind_variables = {"u","v","w"};
const std::string tempname = "temperature_850";

Workspace vortwspc(vorticity_variables, nPoints);
Workspace slpwspc(slpname, nPoints);
Workspace windwspc(wind_variables, nPoints);
Workspace tempwspc(tempname, nPoints);

for (int i=0; i<nPoints; ++i) {
    vortwspc.data.at("vorticity")[i] = (1+i)*1e-4;
    vortwspc.data.at("latitude")[i] = i-5.0;
    slpwspc.data.at("sea_level_pressure")[i] = 990.0+(i%2 == 0 ? -i : 2*i);
    windwspc.data.at("u")[i] = 20.0 + (i<5? i : -i);
    windwspc.data.at("v")[i] = 5.0 + (i<5 ? 0.5*i : -0.5*i);
    windwspc.data.at("w")[i] = 0.01*i;
    tempwspc.data.at("temperature_850")[i] = 300.0 +i;
}

std::cout << "Vorticity workspace: " << vortwspc;

//
//  create ID criteria
//
MinCriterion slp_crit(slpname, 990.0);
MaxCriterion tmpMax_crit(tempname, 308.0);
MaxSignedCriterion vort_crit("vorticity", "latitude", 9e-4);
MaxMagnitudeCriterion wind_crit(wind_variables[0], wind_variables[1], wind_variables[2], 23.0);
MaxAverageCriterion tmpAvg_crit(tempname, 304);
MaxVariationCriterion tmpVar_crit(tempname, 0.5);

//
//  evaluate
//
id_results(&slp_crit, slpwspc);
id_results(&tmpMax_crit, tempwspc);
id_results(&vort_crit, vortwspc);
id_results(&wind_crit, windwspc);
id_results(&tmpAvg_crit, tempwspc);
id_results(&tmpVar_crit, tempwspc);


std::cout << "tests pass." << std::endl;
return 0;
}

void id_results(IDCriterion* idc, const Workspace& wspc) {
    std::cout << idc->description() << " : evaluate() = " << (idc->evaluate(wspc) ? "true" : "false")
        << " : value = " << idc->val << " : index = " << idc->wspcIndex << std::endl;
}
