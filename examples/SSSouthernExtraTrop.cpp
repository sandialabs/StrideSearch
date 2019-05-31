#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include "SSSearchParams.hpp"
#include "SSSearchManager.hpp"
#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace StrideSearch;

typedef LatLonLayout Layout;
typedef std::shared_ptr<IDCriterion> crit_ptr;
typedef std::shared_ptr<CollocationCriterion> colloc_ptr;

struct Input {
    Real sb, nb, wb, eb;
    Real sec_radius;
    Real psl_max;
    Real grad_min;
    std::string data_dir;
    std::string dfilename;
    std::string ofilename;
    Int start_year;
    Int start_month;
    Int start_day;
    Int start_hour;
    Int stop_timestep;
    Int timestep_stride;
    
    Input(const int argc, char* argv[]) : sb(-80), nb(-35), wb(100), eb(200), sec_radius(1000), 
        psl_max(101000), grad_min(0.55), data_dir(StrideSearch_TEST_DATA_DIR), 
        dfilename("ERAinterim_extratrop_grad.nc"), ofilename("so_extrop.txt"),
        start_year(1979), start_month(1), start_day(1), start_hour(0), stop_timestep(-1),
        timestep_stride(60) {
        parse(argc, argv);
    }
    
    inline region_type region() const {return region_type({sb,nb,wb,eb});}
    inline DateTime start_date() const {return DateTime(start_year, start_month, start_day, start_hour);}
    void parse(const int argc, char* argv[]);
    void print_help() const;
};

int main(int argc, char* argv[]) {
    print_copyright();
    Input input(argc, argv);
    std::vector<std::string> fn = {input.data_dir + "/" + input.dfilename};
    
    crit_ptr press_crit(new MinCriterion("SLP", input.psl_max));
    crit_ptr grad_crit(new MaxCriterion("PRESGRAD", input.grad_min));
    //crit_ptr grad_crit(new MaxAverageCriterion("PRESGRAD", "lat", input.grad_min));
    
    std::vector<crit_ptr> criteria = {press_crit, grad_crit};
    
    colloc_ptr pgcolloc(new CollocationCriterion(press_crit, grad_crit, input.sec_radius));
    std::vector<colloc_ptr> coll_criteria = {pgcolloc};
    
    SearchParams params(DTUnits::MINUTES, input.timestep_stride);
    
    SearchManager<Layout> search(input.region(), input.sec_radius);
    search.setStartDate(input.start_date());
    search.setInputFiles(fn, params);
    search.defineCriteria(criteria, coll_criteria);
    std::cout << search.infoString();
    
    search.runSpatialSearch(params, input.stop_timestep);
    
    std::ofstream outfile(input.ofilename);
    search.outputCSV(outfile);
    outfile.close();
}

void Input::parse(const int argc, char* argv[]) {
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
        else if (token == "-psl") {
            psl_max = std::stod(argv[++i]);
        }
        else if (token == "-pg" || token == "-grad") {
            grad_min = std::stod(argv[++i]);
        }
        else if (token == "-h" || token == "--help") {
            print_help();
        }
        else if (token == "-o" || token == "-of") {
            ofilename = argv[++i];
        }
        else if (token == "-i") {
            dfilename = argv[++i];
        }
        else if (token == "-stop") {
            stop_timestep = std::stoi(argv[++i]);
        }
        else if (token == "-ts") {
            timestep_stride = std::stoi(argv[++i]);
        }
    }
}

void Input::print_help() const {
    std::ostringstream ss;
    ss << "southernOceanSearch.\n";
    std::cout << ss.str();
}
