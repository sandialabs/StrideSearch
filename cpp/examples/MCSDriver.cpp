#include "StrideSearchTypeDefs.h"
#include "StrideSearchConfig.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchDataBase.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchMinMaxCriteria.h"
#include "StrideSearchExtraCriteria.h"
#include "StrideSearchTimer.h"
#include "StrideSearchEvent.h"
#include "StrideSearchDateTime.h"
#include "StrideSearchNanoflannTree.h"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>

using namespace StrideSearch;

typedef std::shared_ptr<Event> event_ptr_type;

struct Input{
    Input(int argc, char* argv[]);
    
    std::string outputFilename;
    index_type nInputFiles;
    std::vector<std::string> inFiles;
    std::string program_name;
    DateTime startDate;
    std::string pvarname = "PRECT";
    std::string pvarname1 = "PRECC";
    std::string pvarname2 = "PRECL";
    std::string olrvarname = "FLU";
    int pvarcount;
    
    scalar_type olr_threshold = std::numeric_limits<scalar_type>::max();
    scalar_type precip_threshold = std::numeric_limits<scalar_type>::max();
    scalar_type precip_olr_dist_threshold = std::numeric_limits<scalar_type>::lowest();
    scalar_type west_boundary = 220.0;
    scalar_type east_boundary = 300.0;
    scalar_type south_boundary = 30.0;
    scalar_type north_boundary = 45.0;
    scalar_type sector_size_km = 200.0;
    index_type start_year = 1850;
    index_type start_month = 1;
    index_type start_day = 1;
    index_type start_hour = 0;
    
    void usageMessage(std::ostream &os) const;
    std::string infoString() const;
};

int main(int argc, char* argv[]) {
    print_copyright();
    Timer programTimer("MCSDriver");
    programTimer.start();
    /*
        Get input from command line
    */
    Input input(argc, argv);
    input.usageMessage(std::cout);
    std::cout << std::endl;
    
    std::cout << input.infoString();
    std::cout << std::endl;
    
    Timer setupTimer("MCS_setup");
    setupTimer.start();
    /*
        set up data for reading
    */
    if (input.inFiles.empty()) {
        std::cout << "ERROR: no input files defined." << std::endl;
        return 1;
    }
    std::shared_ptr<StrideSearchData> ssData(new StrideSearchData(input.inFiles[0]));
    // one-time initialization of stride search data, assumes grid is the same for all files
    ssData->initDimensions();
    std::cout << ssData->infoString();
    
    // build tree
    NanoflannTree tree(ssData);
    tree.buildTree();
    
    /*
        set up search region, link to data
    */
    SectorList sectors(input.south_boundary, input.north_boundary, input.west_boundary, 
        input.east_boundary, input.sector_size_km);
    sectors.linkSectorsToData(ssData, tree);
    std::cout << sectors.infoString();
    
    /*
        set up identification criteria
    */
    std::shared_ptr<IDCriterion> olr(new MaxCriterion(input.olrvarname, input.olr_threshold));
    std::shared_ptr<IDCriterion> precip;
    if (input.pvarcount == 1) {
        precip = std::shared_ptr<IDCriterion>(new MaxCriterion(input.pvarname, input.precip_threshold));
    }
    else if (input.pvarcount == 2) {
        precip = std::shared_ptr<IDCriterion>(new 
            MaxSumCriterion(input.pvarname1, input.pvarname2, input.precip_threshold));
    }
    else {
        std::cout << "ERROR: either -precip_var_name or both -precip_var1 and -precip_var2 must be defined." << std::endl;
        return 1;
    }
    std::vector<IDCriterion*> loc_criteria = {precip.get()};
    std::vector<IDCriterion*> id_criteria = {precip.get(), olr.get()};
    
    // collocation criteria
    const scalar_type precip_olr_dist_threshold = input.precip_olr_dist_threshold;
    sectors.buildWorkspaces(loc_criteria);
    setupTimer.end();
    std::cout << "setup complete " << setupTimer.infoString();
    
    /*
        run spatial search 
    */
    Timer searchTimer("MCSDriver_spatial_search");
    searchTimer.start();
    
    EventList mainList;
    //
    //  Loop over each data file
    //
    int fileCounter = 0;
    std::stringstream ss;
    std::string nullstr;
    for (auto& file : input.inFiles) {
        ssData->updateSourceFile(file);
        std::cout << "... processing file " << ++fileCounter << " of " << input.nInputFiles << std::endl;
        
        //
        // Loop over each time step in file
        //
        ss << " of " << ssData->nTimesteps() << "; ";
        const std::string nTimestepsStr = ss.str();
        ss.str(nullstr);
        for (index_type k=0; k < ssData->nTimesteps(); ++k) {
            const DateTime currentDate(ssData->getTime(k), input.startDate);
            std::cout << "\ttimestep " << std::setw(3) << std::setfill(' ') << k 
                << nTimestepsStr << " " << currentDate.DTGString();
            //
            //  loop over each sector using location criteria only
            //
            std::vector<std::vector<event_ptr_type>> possibleEvents;
            int possCounter = 0;
            for (index_type i=0; i<sectors.nSectors(); ++i) {
                ssData->loadSectorWorkingData(sectors.sectors[i].get(), k);
                possibleEvents.push_back(
                    sectors.sectors[i]->evaluateCriteriaAtTimestep(loc_criteria, currentDate, file, k));
                possCounter += possibleEvents[i].size();
            }
            //
            //  create new sectors centered at each possible event
            //
            EventList possibleList(possibleEvents);
            possibleList.removeDuplicates(input.sector_size_km);
            
            SectorList timestepSecList(possibleList, input.sector_size_km);
            timestepSecList.linkSectorsToData(ssData, tree);
            timestepSecList.buildWorkspaces(id_criteria);
            //
            //  loop over possible events, load data, and evaluate all criteria
            //
            int eventCounter = 0;
            std::vector<std::vector<event_ptr_type>> foundEvents;
            for (index_type i=0; i<timestepSecList.nSectors(); ++i) {
                ssData->loadSectorWorkingData(timestepSecList.sectors[i].get(), k);
                foundEvents.push_back(
                    timestepSecList.sectors[i]->evaluateCriteriaAtTimestep(id_criteria, currentDate, file, k));
                eventCounter += foundEvents[i].size();
            }
            EventList foundEventList(foundEvents);
            foundEventList.removeDuplicates(input.sector_size_km);
            //
            //  apply collocation criteria
            //
            foundEventList.consolidateRelatedEvents(input.sector_size_km);
            foundEventList.requireCollocation(precip.get(), olr.get(), input.precip_olr_dist_threshold);
            std::cout << " : " << std::setw(2) << std::setfill(' ') << eventCounter << " events." << std::endl;
            
            //
            //  save timestep data
            //
            mainList.extend(foundEventList);
        }
    }
    searchTimer.end();
    
    //
    // write output
    //
    std::ofstream outfile(input.outputFilename);
    const int indent_tabs = 0;
    const bool printAll = true;
    outfile << mainList.infoString(indent_tabs, printAll);
    outfile.close();
    
    std::cout << setupTimer.infoString();    
    std::cout << searchTimer.infoString(); 
    programTimer.end();
    std::cout << programTimer.infoString();
return 0;
}

Input::Input(int argc, char* argv[]) {
    program_name = argv[0];
    outputFilename = "strideSearchDefaultOutput.txt";
    inFiles = std::vector<std::string>();
    nInputFiles = 0;
    pvarcount = 0;
    if (argc == 1 && argv[1] == "-h") {
        usageMessage(std::cout);   
    }
    if (argc > 1) {
        for (int i=1; i<argc; ++i) {
            const std::string& token = argv[i];
            if (token == "-n") {
                nInputFiles = std::stoi(argv[++i]);
                inFiles.reserve(nInputFiles);
            }
            else if (token == "-o" || token=="--output") {
                outputFilename = argv[++i];
            }
            else if (token == "-east" || token=="--east") {
                east_boundary = std::stod(argv[++i]);
            }
            else if (token == "-south" || token=="--south") {
                south_boundary = std::stod(argv[++i]);
            }
            else if (token == "-west" || token == "--west") {
                west_boundary = std::stod(argv[++i]);
            }
            else if (token == "-north" || token == "--north") {
                north_boundary = std::stod(argv[++i]);
            }
            else if (token == "-precip_var_name") {
                pvarname = argv[++i];
                pvarcount += 1;
            }
            else if (token == "-precip_var1") {
                pvarname1 = argv[++i];
                pvarcount += 1;
            }
            else if (token == "-precip_var2") {
                pvarname2 = argv[++i];
                pvarcount += 1;
            }
            else if (token == "-olr_var_name") {
                olrvarname = argv[++i];
            }
            else if (token == "-precip_threshold") {
                precip_threshold = std::stod(argv[++i]);
            }
            else if (token == "-olr_threshold") {
                olr_threshold = std::stod(argv[++i]);
            }
            else if (token == "-s" || token == "--size") {
                sector_size_km = std::stod(argv[++i]);
            }
            else if (token == "-year") {
                start_year = std::stoi(argv[++i]);
            }
            else if (token == "-month") {
                start_month = std::stoi(argv[++i]);
            }
            else if (token == "-day") {
                start_day = std::stoi(argv[++i]);
            }
            else if (token == "-hour") {
                start_hour = std::stoi(argv[++i]);
            }
            else if (token == "-dist") {
                precip_olr_dist_threshold = std::stod(argv[++i]);
            }
            else {
                inFiles.push_back(token);
            }
        }
        nInputFiles = inFiles.size();
        startDate = DateTime(start_year, start_month, start_day, start_hour);
    }
//     else {
//         usageMessage(std::cerr);
//     }
}

std::string Input::infoString() const {
    std::stringstream ss;
    ss << program_name << " input:" << std::endl;
    ss << "\tsearch boundaries (degrees):" << std::endl;
    ss << "\t\twest = " << west_boundary << ", east = " << east_boundary << std::endl;
    ss << "\t\tsouth = " << south_boundary << ", north = " << north_boundary << std::endl;
    ss << "\tsector radius (km) = " << sector_size_km << std::endl;
    ss << "\tprecipitation threshold (meters per second at surface) = " 
       << precip_threshold << std::endl;
    ss << "\toutgoing longwave radiation threshold (watts/meter^2 at model top) = " 
       << olr_threshold << std::endl;
    ss << "\tprecip max to olr max distance threshold (km) = " << precip_olr_dist_threshold << std::endl;
    ss << "\tdata start date " << startDate.DTGString() << std::endl;
    ss << "\toutput file: " << outputFilename << std::endl;
    return ss.str();
}

void Input::usageMessage(std::ostream& os) const {
    os << "Usage: " << std::endl;
    os << program_name << " -s sector_size_km -olr olr_threshold -precip precip_threshold [-o o_filename.txt] [-n nDataFiles]";
    os << " dataFile1.nc dataFile2.nc ..." << std::endl;
    os << std::endl;
    os << "Required input:" << std::endl;
    os << "\t-precip_threshold (float)" << std::endl;
    os << "\t-precip_var_name (string)" << std::endl;
    os << "\t-olr_threshold (float)" << std::endl;
    os << "\t-olr_varname (string)" << std::endl;
    os << "\t(filename strings)" << std::endl;
    os << "Optional input:" << std::endl;
    os << "\t-o (string) : output filename " << std::endl;
    os << "\t-s or --size (float) : sector radius size (km)" << std::endl;
    os << "\t-north, -south, -west, -east (floats) : search area boundaries (degrees)" << std::endl;
    os << "\t-precip_var1 and -precip_var2 (strings) : used instead of -precip_var_name" << std::endl;
    os << "\t-year, -month, -day, -hour (integers) : data set start date definition." << std::endl;
}

