#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchSector.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchDataBase.h"
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#ifdef USE_NANOFLANN
#include "StrideSearchNanoflannAdaptor.h"
#endif

using namespace StrideSearch;

int main(int argc, char* argv[]) {
    print_copyright();
    
    std::string llGridFile = StrideSearch_TEST_DATA_DIR;
    std::string conusGridFile = StrideSearch_TEST_DATA_DIR;
    std::string csGridFile = StrideSearch_TEST_DATA_DIR;
    
    llGridFile += "/";
    conusGridFile += "/";
    csGridFile += "/";
    
    llGridFile += "sresa1b_ncar_ccsm3-example.nc";
    conusGridFile += "conusx4v1.g";
    csGridFile += "uniform_16_quad4.g";
    
    std::shared_ptr<StrideSearchData> llData(new StrideSearchData(llGridFile));
    std::shared_ptr<StrideSearchData> conusData(new StrideSearchData(conusGridFile));
    std::shared_ptr<StrideSearchData> csData(new StrideSearchData(csGridFile));
    
    llData->initDimensions();
    conusData->initDimensions();
    csData->initDimensions();
    
    NanoflannTree llTree(llData);
    NanoflannTree conusTree(conusData);
    NanoflannTree csTree(csData);

    llTree.buildTree();
    conusTree.buildTree();
    csTree.buildTree();
    
    const scalar_type sb = -90.0;
    const scalar_type nb = 90.0;
    const scalar_type wb = 0.0;
    const scalar_type eb = 360.0;
    
    const scalar_type sector_radius = 3000.0;
    
    SectorList llSectors(sb, nb, wb, eb, sector_radius);
    SectorList conusSectors(sb, nb, wb, eb, sector_radius);
    SectorList csSectors(sb, nb, wb, eb, sector_radius);

    csSectors.linkSectorsToData(csData,csTree);
    std::cout << csSectors.infoString() << std::endl;

    llSectors.linkSectorsToData(llData,llTree);
    std::cout << llSectors.infoString() << std::endl;
    
    conusSectors.linkSectorsToData(conusData,conusTree);
    std::cout << conusSectors.infoString() << std::endl;
    
    std::vector<index_type> plotSectorInds = {23, 39};
    
    for (int i = 0; i < plotSectorInds.size(); ++i) {
        std::stringstream ss;
        ss << "llGridSector" << plotSectorInds[i] << ".csv";
        std::ofstream csvFile(ss.str());
        
        llSectors.sectors[plotSectorInds[i]]->outputCoordsToCSV(csvFile);
    }
    
    for (int i = 0; i < plotSectorInds.size(); ++i) {
        std::stringstream ss;
        ss << "conusGridSector" << plotSectorInds[i] << ".csv";
        std::ofstream csvFile(ss.str());
        
        conusSectors.sectors[plotSectorInds[i]]->outputCoordsToCSV(csvFile);
    }
    
    for (int i = 0; i < plotSectorInds.size(); ++i) {
        std::stringstream ss;
        ss << "csGridSector" << plotSectorInds[i] << ".csv";
        std::ofstream csvFile(ss.str());
        
        csSectors.sectors[plotSectorInds[i]]->outputCoordsToCSV(csvFile);
    }
return 0;
}
