#ifndef _SS_INPUT_HPP_
#define _SS_INPUT_HPP_
#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSUtilities.hpp"
#include <string>
#include "SSDateTime.hpp"

namespace StrideSearch {

/// Base class for handling user input from the command line.
/**
    This class parses command line arguments for use with StrideSearch::SearchManager and
    StrideSearch::SearchParams.
    
    Typically, client driver programs will sublcass this struct for their specific application.
    
    See: SSSouthernExtraTrop.cpp for an example
*/
struct Input {
    /// Default southern boundary of search region (degrees latitude)
    static constexpr Real sb_def = -10;
    /// Default northern boundary of search region (degrees latitude)
    static constexpr Real nb_def = 10;
    /// Default western boundary of search region (degrees longitude)
    static constexpr Real wb_def = 100;
    /// Default eastern boundary of search region (degrees longitude)
    static constexpr Real eb_def = 120;
    /// Default sector radius (kilometers)
    static constexpr Real sr_def = 500;
    /// Default data directory
    static constexpr char data_dir_def[] = StrideSearch_TEST_DATA_DIR;
    /// Default data file name
    static constexpr char pname_def[] = "null";
    /// Default input file name
    static constexpr char filelist_def[] = "null_list.txt";
    /// Default start year
    static constexpr Int sy_def = 1850;
    /// Default start month
    static constexpr Int sm_def = 10;
    /// Default start day
    static constexpr Int sd_def = 1;
    /// Default start hour
    static constexpr Int sh_def = 0;
    /// Default output file name
    static constexpr char ofile_def[] = "ss_output.txt";

    virtual ~Input() {}

    std::string prog_name;
    Real sb, nb, wb, eb;
    Real sec_radius;
    std::string data_dir;
    std::string filelist_fname;
    Int start_year;
    Int start_month;
    Int start_day;
    Int start_hour;
    std::string ofilename;
    DTUnits time_units;
    Int timestep_stride;
    bool valid;
    
    Input() : prog_name(pname_def), sb(sb_def), nb(nb_def), wb(wb_def), eb(eb_def), sec_radius(sr_def),
        data_dir(std::string(data_dir_def)), filelist_fname(filelist_def),
        start_year(sy_def), start_month(sm_def), start_day(sd_def), start_hour(sh_def),
        ofilename(ofile_def), time_units(DTUnits::DAYS), timestep_stride(1), valid(true) {}
        
    Input(int argc, char* argv[]) : Input() {parse_args(argc, argv);}
    
    /// Parse command line input and set member variable values.
    virtual void parse_args(int argc, char* argv[]);
    /// Output a help message
    virtual std::string help_msg() const;
    /// Generate the search region as a StrideSearch::region_type instance
    inline region_type region() const {return region_type({sb, nb, wb, eb});}
    /// Generate the starting date as a StrideSearch::DateTime instance.
    inline DateTime start_date() const {return DateTime(start_year, start_month, start_day, start_hour);}
    /// Generate a vector of data file names (full paths) containing the entier data set.
    std::vector<std::string> dataFilenames() const {return getLinesFromFile(filelist_fname);}
};

}
#endif