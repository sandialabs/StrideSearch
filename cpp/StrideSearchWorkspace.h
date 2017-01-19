#ifndef _STRIDE_SEARCH_WORKSPACE_H_
#define _STRIDE_SEARCH_WORKSPACE_H_

#include <vector>
#include <string>
#include <map>
#include <iostream>

struct Workspace {
    std::map<std::string, std::vector<double> > var_work;
    
    Workspace(std::vector<std::string> keys, const int nValsPerKey);
    
    std::vector<double>& operator[](std::string key) {return var_work[key];}
};
std::ostream& operator<<(std::ostream& os, const Workspace& wspc);

class Workspace2D {
    public :
        typedef double** ptr_type;
    
        int dim0_size;
        int dim1_size;
        bool allocated;
    
        std::map<std::string, ptr_type> data2d;
    
        ptr_type operator[](const std::string key) {return data2d[key];}

        Workspace2D(){dim0_size = 0; dim1_size = 0; allocated = false;}
        Workspace2D(const std::vector<std::string> var_names, const int size_dim0, const int size_dim1);

        Workspace2D& operator=(const Workspace2D& other); // copy assignment
    
        ~Workspace2D(){deleteMemory();}
        
        std::string basicInfo() const;
    private :
        std::vector<std::string> varnames;
        void allocMemory();
        void deleteMemory();
};
std::ostream& operator<<(std::ostream& os, const Workspace2D& wspc);

#endif
