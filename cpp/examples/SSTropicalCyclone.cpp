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
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>
#include <limits>

#ifdef USE_NANOFLANN
#include "StrideSearchNanoflannTree.h"
#endif

using namespace StrideSearch;

struct Input {
    Input(int argc, char* argv[]);
    
    std::string outputFilename;
    index_type nInputfiles;
    std::vector<std::string> inFiles;
    std::string program_name;
    DateTime startDate;
    
    std::string vort_varname = "VOR850";
    scalar_type vorticity_threshold = 0.0;
    
    std::string psl_varname = "PSL";
    scalar_type psl_threshold = std::numeric_limits<scalar_type>::max();
    
    std::string wind_varname1 = "UBOT";
    std::string wind_varname2 = "VBOT";
    scalar_type windspd_threshold = std::numeric_limits<scalar_type>::max();
    
    std::string wc_varname1 = "T500";
    std::string wc_varname2 = "T200";
    scalar_type warm_core_threshold = std::numeric_limits<scalar_type>::max();
    
    scalar_type vort_psl_dist_threshold = 0.0;
    scalar_type temp_psl_dist_threshold = 0.0;
    
    scalar_type west_boundary = 120.0;
    scalar_type east_boundary = 150.0;
    scalar_type south_boundary = -30.0;
    scalar_type north_boundary = 30.0;
    scalar_type sector_size_km = 500.0;
    index_type start_year = 1850;
    index_type start_month = 1;
    index_type start_day = 1;
    index_type start_hour = 0;
    
    bool doTstormsOutput = false;
    
    void usageMessage(std::ostream &os) const;
    std::string infoString() const;
};

int main(int argc, char* argv[]) {
    print_copyright();
    Timer programTimer("TropicalCylconeDriver");
    programTimer.start();
    //should be command line args. Namelist or some other way to read in data.     
    Timer setupTimer("TropicalCyclone_setup");
    setupTimer.start();
    
    //
    //  Set up data set for reading
    //
    //const std::string data_dir = "/gscratch/pabosle/strideSearchData/matt/";
//     const std::string data_dir = "/Users/pabosle/Desktop/dataTemp/StrideSearch/atmLatLon/";
//     const std::string data_filename = "f1850c5_ne240_rel06.cam.h2.0004-07-18-00000.nc";
//     const std::string full_name = data_dir + "/" + data_filename;
//     std::vector<std::string> file_list = {full_name};
    Input input(argc, argv);
    input.usageMessage(std::cout);
    std::cout << input.infoString();
    std::vector<std::string> file_list = input.inFiles;
    
    const DateTime startDate = input.startDate; // Data set initial date = October 1, f1850 compset.
    
    if (file_list.empty()) {
        std::cerr << "ERROR: no input files defined." << std::endl;
        return 1;
    }
    
    std::shared_ptr<StrideSearchData> ssData(new StrideSearchData(file_list[0]));
    ssData->initDimensions();

    
    std::cout << ssData->infoString();
    
    //
    //  Set up search region and StrideSearch sectors
    //
    const scalar_type south_bnd = input.south_boundary;  // southern boundary of search region (degrees north)
    const scalar_type north_bnd = input.north_boundary; // northern boundary of search region (degrees north)
    const scalar_type west_bnd = input.west_boundary; // western boundary of search region (degrees east)
    const scalar_type east_bnd = input.east_boundary; // eastern boundary of search region (degrees east)
    
    const scalar_type sector_size_km = input.sector_size_km; // search sector radius size (kilometers)

    SectorList sectors(south_bnd, north_bnd, west_bnd, east_bnd, sector_size_km);

    NanoflannTree tree(ssData); 

    tree.buildTree();

    sectors.linkSectorsToData(ssData,tree);
    
    std::cout << sectors.infoString(); 
    
    //
    //  Set up identification criteria
    //
    const std::string vort_varname = input.vort_varname;
    const scalar_type vort_threshold = input.vorticity_threshold;
    MaxCriterion vor850(vort_varname, vort_threshold);
    
    const std::string psl_varname = input.psl_varname;
    const scalar_type psl_threshold = input.psl_threshold;
    MinCriterion psl(psl_varname, psl_threshold);
    
    const std::string mid_level_temp = input.wc_varname1;
    const std::string upper_level_temp = input.wc_varname2;
    const scalar_type warm_core_threshold = input.warm_core_threshold;
    MaxVariationCriterionVerticalAvg warm_core(mid_level_temp, upper_level_temp, warm_core_threshold);
    
    const std::vector<std::string> wind_vars = {input.wind_varname1, input.wind_varname2};
    const scalar_type wind_spd_threshold = input.windspd_threshold;
    MaxMagnitude2DCriterion wind_spd(wind_vars, wind_spd_threshold);
    
    std::vector<IDCriterion*> loc_criteria = {&vor850};
    std::vector<IDCriterion*> id_criteria = {&vor850, &psl, &warm_core, &wind_spd};
    
    // collocation criteria
    const scalar_type vort_psl_dist_threshold = input.vort_psl_dist_threshold; // km
    const scalar_type temp_psl_dist_threshold = input.temp_psl_dist_threshold; // km
    
    sectors.buildWorkspaces(loc_criteria);
    
    setupTimer.end();
    std::cout << setupTimer.infoString();
    
    //
    // Run spatial search
    //
    Timer searchTimer("TropicalCylone_spatial_search");
    searchTimer.start();    
    typedef std::shared_ptr<Event> event_ptr_type;
    EventList mainList;
    //
    //  Loop over each data file
    //  (note: this loop is embarrassingly parallel)
    //
    int filecounter = 0;
    std::stringstream ss;
    std::string nullstr;
    int tctr = 0;
    for (auto& file : file_list) {
        ssData->updateSourceFile(file);
        std::cout << "... processing file " << ++filecounter << " of " << file_list.size() << std::endl;
        //
        //  Loop over each time step (note: this loop is embarrassingly parallel)
        //
        ss << " of " << ssData->nTimesteps() << "; ";
        const std::string nTimestepsStr = ss.str();
        ss.str(nullstr);
        for (index_type k = 0; k < ssData->nTimesteps(); ++k) {
            const DateTime currentDate(ssData->getTime(k), startDate);
            std::cout << "timestep " << std::setw(3) << std::setfill(' ') << k << nTimestepsStr << currentDate.DTGString(); 
            //
            //  Loop over each sector, load data, use a single criterion to locate possible events
            //  (note: this loop is embarrassingly parallel)
            //
            std::vector<std::vector<event_ptr_type>> possibleEvents;
            int possCounter = 0;
            for (index_type i = 0; i < sectors.nSectors(); ++i) {
                ssData->loadSectorWorkingData(sectors.sectors[i].get(), k);
                possibleEvents.push_back(
                    sectors.sectors[i]->evaluateCriteriaAtTimestep(loc_criteria, currentDate, file, k));
                possCounter += possibleEvents[i].size();
            }
            //
            //  Create new sectors, one at each possible event location -- search for all criteria
            //
            EventList possibleList(possibleEvents);
            possibleList.removeDuplicates(sector_size_km);
            
            SectorList timestepSecList(possibleList, sector_size_km);
            timestepSecList.linkSectorsToData(ssData,tree);
            timestepSecList.buildWorkspaces(id_criteria);
            //
            //  Loop over possible event sectors, load data, evaluate all criteria 
            //    (note: this loop is embarrassingly parallel)
            //
            int evCounter = 0;
            std::vector<std::vector<event_ptr_type>> foundEvents;
            for (index_type i = 0; i < timestepSecList.nSectors(); ++i) {
                ssData->loadSectorWorkingData(timestepSecList.sectors[i].get(), k);
                foundEvents.push_back(
                    timestepSecList.sectors[i]->evaluateCriteriaAtTimestep(id_criteria, currentDate, file, k));
                evCounter += foundEvents[i].size();
            }
            EventList foundEventList(foundEvents);
            foundEventList.removeDuplicates(sector_size_km);
            //
            //  Apply collocation criteria
            //
            foundEventList.consolidateRelatedEvents(sector_size_km);
            foundEventList.requireCollocation(&vor850, &psl, vort_psl_dist_threshold);
            foundEventList.requireCollocation(&warm_core, &psl, temp_psl_dist_threshold);
            std::cout << " : " << std::setw(2) << std::setfill(' ') << evCounter << " events." << std::endl;   
             
            //
            //  Save timestep data
            //
            if (input.doTstormsOutput) {
                std::stringstream ss;
                ss << input.outputFilename << ".tstorms_" << tctr++ << ".txt";
                std::ofstream tfile(ss.str());
                foundEventList.writeASCIIFormatTSTORMS(tfile);
                tfile.close();
            } 
            mainList.extend(foundEventList);
        }
    }
    searchTimer.end();
    
    //
    //  write output
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
    outputFilename = "strideSearchTropical_defaultOutput.txt";
    psl_varname = "PSL";
    vort_varname = "VOR850";
    wind_varname1 = "UBOT";
    wind_varname2 = "VBOT";
    wc_varname1 = "T500";
    wc_varname2 = "T200";
    start_year = 1850;
    start_month = 1;
    start_day = 1;
    start_hour = 0;
    nInputfiles = 0;
    if (argc == 1 && argv[1] == "-h") {
        usageMessage(std::cout);
    }
    
    if (argc > 1) {
        for (int i=1; i < argc; ++i) {
            const std::string& token = argv[i];
            if (token == "-n") {
                nInputfiles = std::stoi(argv[++i]);
                inFiles.reserve(nInputfiles);
            }
            else if (token == "-o" || token == "--output") {
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
            else if (token == "-vorticity_var_name") {
                vort_varname = argv[++i];
            }
            else if (token == "-vort" or token == "--vorticity") {
                vorticity_threshold = std::stod(argv[++i]);
            }
            else if (token == "-psl_varname") {
                psl_varname = std::stod(argv[++i]);
            }
            else if (token == "--psl_threshold" || token == "-psl") {
                psl_threshold = std::stod(argv[++i]);
            }
            else if (token == "-w1") {
                wind_varname1 = argv[++i];
            }
            else if (token == "-w2") {
                wind_varname2 = argv[++i];
            }
            else if (token == "-wind" || token == "--wind") {
                windspd_threshold = std::stod(argv[++i]);
            }
            else if (token == "-wc1") {
                wc_varname1 = argv[++i];
            }
            else if (token == "-wc2") {
                wc_varname2 = argv[++i];
            }
            else if (token == "-wc" || token == "--warm-core") {
                warm_core_threshold = std::stod(argv[++i]);
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
            else if (token == "-dist1" || token == "-vort-psl-dist") {
                vort_psl_dist_threshold = std::stod(argv[++i]);
            }
            else if (token == "-dist2" || token == "-wc-psl-dist") {
                temp_psl_dist_threshold = std::stod(argv[++i]);
            }
            else if (token == "-tstorms") {
                doTstormsOutput = true;
            }
            else {
                inFiles.push_back(token);
            }
        }
    }
    if (vort_psl_dist_threshold == 0.0) vort_psl_dist_threshold = sector_size_km;
    if (temp_psl_dist_threshold == 0.0) temp_psl_dist_threshold = sector_size_km;
    nInputfiles = inFiles.size();
    startDate = DateTime(start_year, start_month, start_day, start_hour);
}

void Input::usageMessage(std::ostream& os) const {
    os << "Usage: " << std::endl;
    os << program_name << " -[s,year,month,day,hour,dist1,dist2,vorticity_threshold,vort_varname,psl,psl_varname,wc]"; 
    os << " dataFile1.nc dataFile2.nc ..." << std::endl;
    os << "Required input: " << std::endl;
    os << "\t-psl psl_threshold (float) " << std::endl;
    os << "\t-wc warm core temperature excess threshold (float)" << std::endl;
    os << "Optional input: " << std::endl;
    os << "\t-o (string) : output filename " << std::endl;
    os << "\t-s or --size (float) : sector radius size (km)" << std::endl;
    os << "\t-north, -south, -west, -east (floats) : search area boundaries (degrees)" << std::endl;
    os << "\t-vort_varname (string) : vorticity variable name in .nc file" << std::endl;
    os << "\t-wind_varname1, -wind_varname2 (strings) : wind component variable names in .nc file"<< std::endl;
    os << "\t-wind (float) : wind speed threshold" << std::endl;
    os << "\t-psl_varname (string) : sea level pressure variable name " << std::endl;
    os << "\t-wc1, -wc2 (strings) : temperature level variable names for use in warm core criteria" << std::endl;
    os << "\t-year, -month, -day, -hour (integers) : data set start date definition." << std::endl;
    os << "\t-dist1 (float)" << std::endl;
    os << "\t-dist2 (fload)" << std::endl;
}

std::string Input::infoString() const {
    std::stringstream ss;
    ss << program_name << " input:" << std::endl;
    ss << "\t\twest = " << west_boundary << ", east = " << east_boundary << std::endl;
    ss << "\t\tsouth = " << south_boundary << ", north = " << north_boundary << std::endl;
    ss << "\tsector radius (km) = " << sector_size_km << std::endl;
    ss << "\tvorticity variable: " << vort_varname << ", threshold = " << vorticity_threshold << std::endl;
    ss << "\tpsl variable: " << psl_varname << ", threshold  = " << psl_threshold << std::endl;
    ss << "\twarm core variables: " << wc_varname1 << ", " << wc_varname2 << "; threshold = " << warm_core_threshold << std::endl;
    ss << "\tcollocation distance (vortmax, pslmin) = " << vort_psl_dist_threshold << std::endl;
    ss << "\tcollocation distance (warm core, plsmin) = " << temp_psl_dist_threshold << std::endl;
    ss << "\twind speed variables: " << wind_varname1 << ", " << wind_varname2 << "; threshold = " << windspd_threshold << std::endl;
    ss << "\tdata start date " << startDate.DTGString() << std::endl;
    ss << "\toutput file: " << outputFilename << std::endl;
    ss << "\tnInputfiles = " << nInputfiles << std::endl;
    return ss.str();
}

