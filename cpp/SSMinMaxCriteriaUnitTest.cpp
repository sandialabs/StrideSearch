#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchEvent.h"
#include "StrideSearchWorkspaceDict.h"

using namespace StrideSearch;

int main() {
    
    //
    // create sample workspaces
    //
    const int nPoints = 10;
    std::vector<std::string> vortvarnames = {"vorticity", "latitude"};
    std::string slpvarname = "sea_level_pressure";
    std::vector<std::string> wind3dvarnames = {"u", "v", "w"};
    std::vector<std::string> horizWindVarnames = {"u", "v"};
    
    WorkspaceDict vorWspc(vortvarnames, nPoints);
    WorkspaceDict slpWspc(slpvarname, nPoints);
    WorkspaceDict wnd2dWspc(horizWindVarnames, nPoints);
    WorkspaceDict wnd3dWspc(wind3dvarnames, nPoints);
    WorkspaceDict tempWspc("temperature", nPoints);
    
    for (int i = 0; i < nPoints; ++i) {
        vorWspc.dict.at("vorticity")[i] = (1.0 + i) * 1.0e-4;
        vorWspc.dict.at("latitude")[i] = i - 5.0;
        slpWspc.dict.at("sea_level_pressure")[i] = 990.0 + ( i%2 == 0 ? -i : 2*i);
        
        const scalar_type uu = 20.0 + (i < 5 ? i : -i);
        const scalar_type vv = 5.0 + (i < 5 ? i/2.0 : -i/2.0);
        const scalar_type ww = i/100.0;
        
        wnd2dWspc.dict.at("u")[i] = uu;
        wnd2dWspc.dict.at("v")[i] = vv;
        wnd3dWspc.dict.at("u")[i] = uu;
        wnd3dWspc.dict.at("v")[i] = vv;
        wnd3dWspc.dict.at("w")[i] = ww;
        
        tempWspc.dict.at("temperature")[i] = 300.0 + i;
    }

    //
    //  create id criteria
    //
    MinCriterion slpCrit(slpvarname, 990.0);
    MaxCriterion tmpMaxCrit("temperature", 308.0);
    MaxSignedCriterion vortCrit("vorticity", "latitude", 9.0e-4);
    MaxMagnitude2DCriterion horizWndCrit("u", "v", 22.0);
    MaxMagnitude3DCriterion wndSpdCrit(wind3dvarnames, 23.0);
    MaxAverageCriterion avgTempCrit("temperature", 304.0);
    MaxVariationCriterion varTempCrit("temperature", 0.5);
    
    std::cout << "slpCrit.description = " << slpCrit.description() << std::endl;
    std::cout << "slpCrit.evaluate = " << (slpCrit.evaluate(slpWspc) ? "true" : "false") << std::endl;
    
    std::cout << "tmpMaxCrit.description() = " << tmpMaxCrit.description() << std::endl;
    std::cout << "tmpMaxCrit.evaluate = " << (tmpMaxCrit.evaluate(tempWspc) ? "true" : "false") << std::endl;
    
    std::cout << "vortCrit.description() = " << vortCrit.description() << std::endl;
    std::cout << "vortCrit.evaluate = " << (vortCrit.evaluate(vorWspc) ? "true" : "false") << std::endl;
    
    std::cout << "horizWndCrit.description() = " << horizWndCrit.description() << std::endl;
    std::cout << "horizWndCrit.evaluate = " << (horizWndCrit.evaluate(wnd2dWspc) ? "true" : "false") << std::endl;
    
    std::cout << "wndSpdCrit.evaluate = " << wndSpdCrit.description() << std::endl;
    std::cout << "wndSpdCrit.evaluate = " << (wndSpdCrit.evaluate(wnd3dWspc) ? "true" : "false" ) << std::endl;
    
    std::cout << "avgTempCrit.evaluate = " << avgTempCrit.description() << std::endl;
    std::cout << "avgTempCrit.evaluate = " << (avgTempCrit.evaluate(tempWspc) ? "true" : "false" ) << std::endl;
    
    std::cout << "varTempCrit.evaluate = " << varTempCrit.description() << std::endl;
    std::cout << "varTempCrit.evaluate = " << (varTempCrit.evaluate(tempWspc) ? "true" : "false" ) << std::endl;
    
    std::cout << "slpCrit (ind, val) = (" << slpCrit.wspcIndex << ", " << slpCrit.val << ")\n";
    std::cout << "tmpMaxCrit (ind, val) = (" << tmpMaxCrit.wspcIndex << ", " << tmpMaxCrit.val << ")\n";
    std::cout << "vortCrit (ind, val) = (" << vortCrit.wspcIndex << ", " << vortCrit.val << ")\n";
    std::cout << "horizWndCrit (ind, val) = (" << horizWndCrit.wspcIndex << ", " << horizWndCrit.val << ")\n";
    std::cout << "wndSpdCrit (ind, val) = (" << wndSpdCrit.wspcIndex << ", " << wndSpdCrit.val << ")\n";
    std::cout << "avgTempCrit (ind, val) = (" << avgTempCrit.wspcIndex << ", " << avgTempCrit.val << ")\n";
    std::cout << "varTempCrit (ind, val) = (" << varTempCrit.wspcIndex << ", " << varTempCrit.val << ")\n";
return 0;
}

