#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSSearchManager.hpp"
#include "SSIdCriteria.hpp"
#include "SSCollocCriteria.hpp"
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

struct Input {
    Real sb, nb, wb, eb;
    Real sec_radius;
    Real zeta_min;
    Real psl_max;
    Real wind_min;
    Real warm_core_min;
    Real warm_core_dist;
    Real circ_center_dist;
    std::string data_dir;
    Int start_year;
    Int start_month;
    Int start_day;
    Int start_hour;
    std::string filelist_fname;
    std::string ofilename;
    
    Input() : sb(-30), nb(30), wb(90), eb(200), sec_radius(500), zeta_min(8.5e-4), psl_max(99000), wind_min(10),
        warm_core_min(2), warm_core_dist(225), circ_center_dist(450), data_dir(StrideSearch_TEST_DATA_DIR),
        start_year(1851), start_month(10), start_day(1), start_hour(0), ofilename("exampleOutput.txt"),
        filelist_fname("../examples/tc_datafiles.txt") {}
    
    void parse_args(int argc, char* argv[]);
    void print_help() const;
    inline region_type region() const {return region_type({sb,nb,wb,eb});}
    inline DateTime start_date() const {return DateTime(start_year, start_month, start_day, start_hour);}
    std::vector<std::string> getFilenames() const {return getLinesFromFile(filelist_fname);}
};

int main(int argc, char* argv[]) {
    print_copyright();
    /// Step 1: Collect user input
    Input input;
    input.parse_args(argc, argv);

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
    search.setInputFiles(input.getFilenames());
    search.defineCriteria(criteria, colloc_criteria);
    std::cout << search.infoString();
    
    /// Step 4: Run the search
    search.runSpatialSearch();
    
    /// Step 5: Output results
    std::ofstream csvfile(input.ofilename);
    search.outputCSV(csvfile);
    csvfile.close();
    
return 0;
}

void Input::parse_args(int argc, char* argv[]) {
    for (int i=1; i<argc; ++i) {
        const std::string& token = argv[i];
        if (token == "-sb") {
            sb = std::stod(argv[++i]);
        }
        else if (token == "-nb") {
            nb = std::stod(argv[++i]);
        }
        else if (token == "-wb") {
            wb = std::stod(argv[++i]);
        }
        else if (token == "-eb") {
            eb = std::stod(argv[++i]);
        }
        else if (token == "-sr") {
            sec_radius = std::stod(argv[++i]);
        }
        else if (token == "-vor") {
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
            print_help();
        }
        else if (token == "-o" || token == "-of") {
            ofilename = argv[++i];
        }
        else if (token == "-i") {
            filelist_fname = argv[++i];
        }
    }
}

void Input::print_help() const {
    std::ostringstream ss;
    ss << "TODO: Write this message.\n";
    std::cout << ss.str();
}



