#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSSearchManager.hpp"
#include "SSIdCriteria.hpp"
#include "SSCollocCriteria.hpp"
#include "SSInput.hpp"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>
/**
    @file 
    Basic example of a serial tropical cyclone search driver.
    Input is from the command line, with defaults for each variable predefined in the Input struct's constructor.
    
    The ::main function illustrates the StrideSearch workflow and how to use the StrideSearch::SearchManager class.
*/
using namespace StrideSearch;

typedef LatLonLayout Layout;
typedef std::shared_ptr<IDCriterion> crit_ptr;
typedef std::shared_ptr<CollocationCriterion> colloc_ptr;

struct TCInput : public Input {
    Real zeta_min;
    Real psl_max;
    Real wind_min;
    Real warm_core_min;
    Real warm_core_dist;
    Real circ_center_dist;
    Int stop_timestep;
    
    TCInput(int argc, char* argv[]) : Input() {parse_args(argc, argv);}
    
    void parse_args(int argc, char* argv[]) override;
    std::string help_msg() const override;
    std::string status_msg() const;
};

int main(int argc, char* argv[]) {
    print_copyright();
    /// Step 1: Collect user input
    TCInput input(argc, argv);
    std::cout << input.status_msg();

    /// Step 2: Define a storm
    crit_ptr vor850(new MaxSignedCriterion("VOR850", "lat", input.zeta_min));
    crit_ptr psl(new MinCriterion("PSL", input.psl_max));
    crit_ptr sfcwind(new MaxMagnitudeCriterion("UBOT", "VBOT", input.wind_min));
    crit_ptr warmcore(new MaxVariationOfAverageCriterion("T200", "T500", input.warm_core_min));
    
    const std::vector<crit_ptr> criteria = {vor850, psl, warmcore, sfcwind};
    
    colloc_ptr pslWarmCoreColloc(new CollocationCriterion(psl, warmcore, input.warm_core_dist));
    colloc_ptr pslVortColloc(new CollocationCriterion(vor850, psl, input.circ_center_dist));
    
    const std::vector<colloc_ptr> colloc_criteria = {pslVortColloc, pslWarmCoreColloc};    

    /// Step 3: Setup the search
    SearchManager<Layout> search(input.region(), input.sec_radius);
    search.setStartDate(input.start_date());
    search.setInputFiles(input.dataFilenames());
    search.defineCriteria(criteria, colloc_criteria);
    std::cout << search.infoString();
    
    /// Step 4: Run the search
    search.runSpatialSearch();
    
    /// Step 5: Output results
    std::ofstream csvfile(input.ofilename);
    search.outputCSV(csvfile);
    csvfile.close();
    
    std::vector<std::string> descs;
    for (auto& c : criteria) {
        descs.push_back(c->description());
    }
    search.buildTracks(15, 6, 2);
    search.outputTracks("ss_tc_example", descs);
    
return 0;
}

void TCInput::parse_args(int argc, char* argv[]) {
    Input::parse_args(argc, argv);
    sb = -30;
    nb = 30;
    wb = 100;
    eb = 160;
    sec_radius = 500;
    data_dir = StrideSearch_TEST_DATA_DIR;
    filelist_fname = "../examples/tc_datafiles.txt";
    start_year = 1851;
    start_month = 10;
    start_day = 1;
    start_hour = 0;
    time_units = HOURS;
    zeta_min = 8.5e-4;
    psl_max = 99000.0;
    wind_min = 10.0;
    warm_core_min = 2;
    warm_core_dist = 225;
    circ_center_dist = 450;
    for (int i=1; i<argc; ++i) {
        const std::string& token = argv[i];
        if (token == "-vor") {
            zeta_min = std::stod(argv[++i]);
        }
        else if (token == "-psl") {
            psl_max = std::stod(argv[++i]);
        }
        else if (token == "-wc") {
            warm_core_min = std::stod(argv[++i]);
        }
        else if (token == "-wcdist") {
            warm_core_dist = std::stod(argv[++i]);
        }
        else if (token == "-circ") {
            circ_center_dist = std::stod(argv[++i]);
        }
        else if (token == "-h" || token == "--help") {
            std::cout << help_msg();
        }
        else if (token == "-wind") {
            wind_min = std::stod(argv[++i]);
        }
    }
}

std::string TCInput::help_msg() const {
    std::ostringstream ss;
    ss << Input::help_msg();
    ss << "\t-vor <vorticity threshold> \t relative vorticity threshold [default: 8.5e-4 s^{-1}]\n";
    ss << "\t-psl <sea level pressure threshold> \t pressure threshold [default: 99000.0 Pa]\n";
    ss << "\t-wc <warm core temperature threshold> \t temperature excess threshold [default: 2 K]\n";
    ss << "\t-wcdist <psl_min to warm core correlation distance> [default: 250 km]\n";
    ss << "\t-circ <psl_min to vorticity_max correlation distance> [default: 450 km]\n";
    ss << "\t-wind <wind speed min.> horizontal wind speed threshold [default 10 mps]\n";
    return ss.str();
}

std::string TCInput::status_msg() const {
    std::ostringstream ss;
    ss << prog_name << " input summary:\n";
    ss << "\tsearch region = " << region() << '\n';
    ss << "\tsector radius = " << sec_radius << "km\n";
    ss << "\tdata_dir = " << data_dir << '\n';
    ss << "\tfilelist_fname = " << filelist_fname << '\n';
    ss << "\tstart_date = " << start_date().isoFullStr() << '\n';
    ss << "\toutput file = " << ofilename << '\n';
    ss << "\tvorticity threshold = " << zeta_min << " s^{-1}\n";
    ss << "\tpsl_threshold = " << psl_max << " Pa\n";
    ss << "\temperature excess threshold = " << warm_core_min << " deg C\n";
    ss << "\twarm core colloc. distance = " << warm_core_dist << " km\n";
    ss << "\tpsl-vorticity distance = " << circ_center_dist << " km\n";
    ss << "\twindspeed threshold = " << wind_min << " m s^{-1}\n";
    return ss.str();
}

