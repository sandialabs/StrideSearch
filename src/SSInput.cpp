#include "SSInput.hpp"
#include <sstream>
#include <exception>

namespace StrideSearch {

constexpr char Input::data_dir_def[];
constexpr char Input::pname_def[];
constexpr char Input::filelist_def[];
constexpr char Input::ofile_def[];

void Input::parse_args(int argc, char* argv[]) {
    prog_name = argv[0];
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
        else if (token == "-h" || token == "--help") {
            std::cout << help_msg();
        }
        else if (token == "-o" || token == "-of") {
            ofilename = argv[++i];
        }
        else if (token == "-i") {
            filelist_fname = argv[++i];
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
        else if (token == "-data") {
            data_dir = argv[++i];
        }
        else if (token == "-time") {
            const std::string unitstr(argv[++i]);
            if (unitstr == "days") {
                    time_units = DTUnits::DAYS;
            }
            else if (unitstr == "hours") {
                    time_units = DTUnits::HOURS;
            }
            else if (unitstr == "minutes") {
                time_units = DTUnits::MINUTES;
            }
            else {
                std::ostringstream ss;
                ss << help_msg();
                ss << "Input::parse_args error: unsupported time unit " << unitstr << ". See SSDateTime.hpp.\n";
                valid = false;
                throw std::invalid_argument(ss.str());
            }
        }
        else if (token == "-ts") {
            timestep_stride = std::stoi(argv[++i]);
        }
    }
}

std::string Input::help_msg() const {
    std::ostringstream ss;
    ss << prog_name << "\n\n";
    ss << "Usage:\n";
    ss << '\t' << prog_name << " [options]\n";
    ss << '\n';
    ss << "Options:\n";
    ss << "\t-h, --help \t\t Show this message.\n";
    ss << "\t-sb <south_boundary> \t southern boundary of search region, in degrees [-90,90) [default: " << sb_def << "]\n";
    ss << "\t-nb <north_boundary> \t northern boundary of search region, in degrees (-90,90] [default: " << nb_def <<"]\n";
    ss << "\t-wb <west_boundary> \t western boundary of search region, in degrees [0,360) [default: " << wb_def << "]\n";
    ss << "\t-eb <east_boundary> \t eastern boundary of search region, in degrees (0,360] [default: " << eb_def << "]\n";
    ss << "\t-sr <sector_radius> \t spatial search sector radius in kilometers [default: " << sr_def << "]\n";
    ss << "\t-data <data_dir> \t path to data directory [default: " << data_dir_def << "]\n";
    ss << "\t-i <filelist_filename> \t filename of a text file containing input data filenames [default: " << filelist_def << "]\n";
    ss << "\t-o <output filename> \t name for StrideSearch output file. [default: " << ofile_def << "]\n";
    ss << "\t-year <start_year> \t starting year of data set [default: " << sy_def << "]\n";
    ss << "\t-month <start_month> \t starting month of data set [default: " << sm_def << "]\n";
    ss << "\t-day <start_day> \t starting day of data set [default: " << sd_def << "]\n";
    ss << "\t-hour <start_hour> \t starting hour of data set [default: " << sh_def << "]\n";
    ss << "\t-time <unit> \t\t unit of time for data set [default: DAYS]\n";
    ss << "\t-ts <timestep_stride> \t time step increment for data search [default: 1]\n";
    return ss.str();
}

}