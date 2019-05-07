#ifndef _SS_DATA_HPP_
#define _SS_DATA_HPP_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSNCReader.hpp"
#include "SSKdTree.hpp"

namespace StrideSearch {

class SSData {
    public:
        Points points;
        tree_type tree;
        
        static constexpr Int max_leaf = 20;
        
        void updateFile(const std::string& fn) {
            reader->updateFile(fn);
            time = reader->getTime();
        }
        
        std::string getFilename() const {return reader->filename();}
        
        Int nTimestepsInFile() const {return time.size();}
        
        RealArray time;
        
        SSData(std::shared_ptr<NCReader> rdr);
        
    protected:
        std::shared_ptr<NCReader> reader;
        adaptor_type adaptor;
};

}
#endif