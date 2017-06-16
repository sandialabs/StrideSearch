#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include <vector>
#include "StrideSearchWorkspaceDict.h"
#include "StrideSearchSector.h"
#include "StrideSearchSectorList_Base.h"
#include "StrideSearchSectorListLatLon.h"

using namespace StrideSearch;

int main() {

    const scalar_type dataResDeg = 10.0;
    const index_type nLat = 19;
    const index_type nLon = 36;
    
    const scalar_type wb = 100.0;
    const scalar_type eb = 190.0;
    const scalar_type sb = -30.0;
    const scalar_type nb = 30.0;
    
    const scalar_type rad = 2200.0;
    
    SectorListLatLon secList(sb, nb, wb, eb, rad, nLat, nLon);
    
    std::cout << secList.infoString();
    
    if ( secList.nSectors() < 20) {
        for (int i = 0; i < secList.nSectors(); ++i)
            std::cout << secList.sectorInfoString(i);
    }
    else {
        std::cout << secList.sectorInfoString(0);
        std::cout << secList.sectorInfoString(1);    
    }
return 0;
}
