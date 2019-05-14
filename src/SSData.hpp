#ifndef _SS_DATA_HPP_
#define _SS_DATA_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSNCReader.hpp"
#include "SSKdTree.hpp"
#include "SSSector.hpp"
#include "SSSectorSet.hpp"
#include <memory>
#include <string>

namespace StrideSearch {

/// Foundational interface to netCDF data files.
/**
    @tparam ReaderType : subclass of NCReader matched to the data files' grid type
*/
template <typename ReaderType>
class SSData {
    public:
        typedef typename ReaderType::Layout Layout;
        /// KDTree used for fast searching 
        KDTree tree;
        /// Time values contained in the current file
        RealArray time;
        /// Return the current filename
        std::string getFilename() const {return reader->filename();}
        
        /// Constructor; initializes its own netCDF reader, builds tree, loads time values from first file
        SSData(const std::string& fname);
        
        /// Called for each file in the data set after the first
        /**
            Updates the reader to the next file, loads time values.
            @note Each file in a data set are assumed to use the same spatial grid
            @param fn : filename to process next
        */
        void updateFile(const std::string& fn) {
            reader->updateFile(fn);
            time = reader->getTime();
        }
        
        Int nTimestepsInFile() const {return time.size();}
        
        /// Define the data points contained within each Sector in a SectorSet
        /**
            Uses nanoflann.hpp tpl for fast searching.
            @param secset : SectorSet whose centers are defined but its Sector definitions are incomplete
            @post : secset has completely defined Sectors
        */
        void linkSectorsToData(SectorSet<Layout>& secset);
        
        /// Load data into a Sector's work space from the specified time index of the current data filename
        /**
            @param sec : Sector whose Workspace will be updated
            @param time_ind : time index to read from current file
            @param lev_ind : (if applicable) level index to read from current file
        */
        void loadSectorWorkingData(Sector<Layout>& sec, const Index& time_ind, const Index& lev_ind=-1) const;
        
    protected:
        std::unique_ptr<NCReader> reader;
        adaptor_type adaptor;
        void loadTimeVariable() {time = reader->getTime();}
};

}
#endif