#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include "SSSearchParams.hpp"
#include "SSSearchManager.hpp"
#include "SSInput.hpp"
#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>

/**
    @file 
    Basic example of a serial extratropical cyclone search driver.
    Input is from the command line, with defaults for each variable predefined in the Input struct's constructor.
    
    Criteria from Mendes et. al. 2010, _Theor. Appl. Climatology_ 100.
    
    The ::main function illustrates the StrideSearch workflow and how to use the StrideSearch::SearchManager class.
*/

using namespace StrideSearch;

typedef LatLonLayout Layout;
typedef std::shared_ptr<IDCriterion> crit_ptr;
typedef std::shared_ptr<CollocationCriterion> colloc_ptr;

struct ExTropInput : public Input {
    Real psl_max;
    Real grad_min;
    Int stop_timestep;
    Int timestep_stride;

    ExTropInput(int argc, char* argv[]): Input(argc, argv) {parse_args(argc, argv);}

    void parse_args(const int argc, char* argv[]) override;
    std::string help_msg() const override;
    std::string status_msg() const;
};

int main(int argc, char* argv[]) {
    print_copyright();
    /// Step 1: Collect user input
    ExTropInput input(argc, argv);
    std::cout << input.status_msg();
    if (!input.valid) {
        throw std::runtime_error("invalid input.");
    }
    std::vector<std::string> fn = {input.data_dir + "/" + input.filelist_fname};
    
    crit_ptr press_crit(new MinCriterion("SLP", input.psl_max));
    //crit_ptr grad_crit(new MaxCriterion("PRESGRAD", input.grad_min));
    crit_ptr grad_crit(new MaxAverageCriterion("PRESGRAD", "lat", input.grad_min));
    
    std::vector<crit_ptr> criteria = {press_crit, grad_crit};
    
    colloc_ptr pgcolloc(new CollocationCriterion(press_crit, grad_crit, input.sec_radius));
    std::vector<colloc_ptr> coll_criteria = {pgcolloc};
    
    /** Step 2: Define search parameters.  
        This is required because this data set has time values define in units of minutes, not days.
        Also, using SearchParams::timestep_stride is a good way to speed up the search.
    */
    SearchParams params(input.time_units, input.timestep_stride);
    
    /// Step 3: Setup the search
    SearchManager<Layout> search(input.region(), input.sec_radius);
    search.setStartDate(input.start_date());
    search.setInputFiles(fn, params);
    search.defineCriteria(criteria, coll_criteria);
    std::cout << search.infoString();
    
    /// Step 4: Run the search
    search.runSpatialSearch(params, input.stop_timestep);
    
    /// Step 5: Output results
    std::ofstream outfile(input.ofilename);
    search.outputCSV(outfile);
    outfile.close();
}

void ExTropInput::parse_args(const int argc, char* argv[]) {
    filelist_fname = "ERAinterim_extratrop_grad.nc";
    sb = 20;
    nb = 40;
    wb = 280;
    eb = 310;
    sec_radius = 500;
    psl_max = 99000;
    grad_min = 0.22;
    time_units = DTUnits::HOURS;
    timestep_stride = 1;
    stop_timestep = 124;
    start_year = 1900;
    start_month = 1;
    ofilename = "so_extratrop_results.txt";
    for (int i=1; i<argc; ++i) {
        const std::string& token = argv[i];
        if (token == "-psl") {
            psl_max = std::stod(argv[++i]);
        }
        else if (token == "-pg" || token == "-grad") {
            grad_min = std::stod(argv[++i]);
        }
        else if (token == "-stop") {
            stop_timestep = std::stoi(argv[++i]);
        }
        else if (token == "-h" | token == "--help") {
            std::cout << help_msg();
            valid = false;
        }
    }
}

std::string ExTropInput::status_msg() const {
    std::ostringstream ss;
    ss << prog_name << " input summary:\n";
    ss << "\tsearch region = " << region() << '\n';
    ss << "\tsector radius = " << sec_radius << " km\n";
    ss << "\tdata_dir = " << data_dir << '\n';
    ss << "\tfilelist_fname = " << filelist_fname << '\n';
    ss << "\tstart date = " << start_date().isoFullStr() << '\n';
    ss << "\toutput file = " << ofilename << '\n';
    return ss.str();
}

std::string ExTropInput::help_msg() const {
    std::ostringstream ss;
    ss << Input::help_msg();
    ss << "\t-psl <psl threshold> \t Sea level pressure threshold in Pascals.\n";
    ss << "\t-grad <press. gradient threshold> \t Pressure gradient threshold in Pa per km.\n";
    ss << "\t-stop <timestep number> \t Stop search at this time step [default: -1].\n";
    return ss.str();
}
