#include "StrideSearchUtilities.h"
#include "StrideSearchWorkspace.h"
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

Workspace::Workspace(std::vector<std::string> keys, const int nValsPerKey){
    for (int i = 0; i < keys.size(); ++i) {
        var_work[keys[i]] = std::vector<double>(nValsPerKey);
    }
}

Workspace2D::Workspace2D(const std::vector<std::string> var_names, const int size_dim0, const int size_dim1){
    dim0_size = size_dim0;
    dim1_size = size_dim1;
    std::cout << "testing" << "\n";
    varnames = var_names;
    allocated = false;
    allocMemory();
}

std::ostream& operator<<(std::ostream& os, const Workspace& wspc){
    for (auto& elem : wspc.var_work){
        os << elem.first << " = [";
        for (int i = 0; i < elem.second.size(); ++i){
            os << elem.second[i];
            if (i < elem.second.size() - 1)
                os << ", ";
            else
                os << "]\n";
        }
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const Workspace2D& wspc) {
    os << "dim0_size = " << wspc.dim0_size << ", dim1_size = " << wspc.dim1_size << " ";
    os << "allocated = " << (wspc.allocated ? "true" : "false") << std::endl;
    if (wspc.allocated){
        for (auto& elem : wspc.data2d) {
            std::stringstream ss;
            ss << elem.first << " = [";
            std::string idstring = ss.str();
            const int space_len = idstring.size();
        
            os << idstring;
        
            for (int i = 0; i < wspc.dim0_size; ++i){
                if (i > 0 )
                    os << std::setw(space_len) << std::setfill(' ') << " ";
                for (int j = 0; j < wspc.dim1_size; ++j){
                    os << elem.second[i][j];
                    if (j < wspc.dim1_size - 1)
                        os << ", ";
                    else {
                        if (i < wspc.dim0_size - 1)
                            os << ";\n";
                        else
                            os << "]\n";
                    }                    
                }    
            }
        }
    }
    return os;
}

std::string Workspace2D::basicInfo() const {
    std::stringstream ss;
    ss << "allocated = " << (allocated ? "true " : "false ") << ", dim0_size = " << dim0_size << 
        ", dim1_size = " << dim1_size << std::endl;
    return ss.str();
}

void Workspace2D::allocMemory(){
//     std::cout << "ALLOC MEMORY entry, allocated = " << (allocated ? "true " : "false ") << std::endl;
    if (dim0_size > 0 && dim1_size > 0) {
        for (int key = 0; key < varnames.size(); ++key){
            data2d[varnames[key]] = new double*[dim0_size];
            for (int i = 0; i < dim0_size; ++i) {
                data2d[varnames[key]][i] = new double[dim1_size];
            }
        }
        allocated = true;
    }
}

void Workspace2D::deleteMemory(){
//     std::cout << "DELETE MEMORY entry, allocated = " << (allocated ? "true " : "false ") << std::endl;
    if (allocated) {
        for (auto& elem : data2d) {
            for (int i = 0; i < dim0_size; ++i)
                delete[] elem.second[i];
            delete[] elem.second;
        }
        allocated = false;
        dim0_size = 0;
        dim1_size = 0;
    }
}

Workspace2D& Workspace2D::operator=(const Workspace2D& other){
//     std::cout << "OPERATOR = entry, allocated = " << (allocated ? "true " : "false ") << std::endl;
    if (this != &other){
        if (this->allocated)
            deleteMemory();
        this->dim0_size = other.dim0_size;
        this->dim1_size = other.dim1_size;
        this->varnames = other.varnames;
        this->allocMemory();

        for (auto&elem : other.data2d){
            ptr_type destData = this->data2d[elem.first];
            for (int i = 0; i < this->dim0_size; ++i) {
                for (int j = 0; j < this->dim1_size; ++j) {
                    destData[i][j] = elem.second[i][j];
                }
            }
        }

    }
    return *this;
}
