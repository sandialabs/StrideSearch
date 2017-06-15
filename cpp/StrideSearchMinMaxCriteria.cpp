#include "StrideSearch_Config.h"
#include "StrideSearch_TypeDefs.h"
#include "StrideSearchMinMaxCriteria.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cmath>

namespace StrideSearch {


bool MinCriterion::evaluate(const WorkspaceDict& wspc)
 {
    val = 1.e20;
    // find the minimum data value in the workspace
    //std::vector<scalar_type> varvec = wspc.dict.at(varnames[0]);
    const std::vector<scalar_type>& dataRef = wspc.dict.at(varnames[0]);
    for (index_type i = 0; i < dataRef.size(); ++i) {
        if ( dataRef[i] < val) {
            wspcIndex = i;
            val = dataRef[i];
        }       
    }
    return (val < threshold);
}

std::string MinCriterion::description() const {
    return "min(" + varnames[0] + ")";
}

bool MaxCriterion::evaluate(const WorkspaceDict& wspc) {
    val = -1.0e20;
    // find the maximum data value in the workspace
    const std::vector<scalar_type>& dataRef = wspc.dict.at(varnames[0]);
    for (index_type i = 0; i < dataRef.size(); ++i) {
        if (wspc.dict.at(varnames[0])[i] > val) {
            wspcIndex = i;
            val = dataRef[i];
        }
    }
    return (val > threshold);
}

std::string MaxCriterion::description() const {
    return "max(" + varnames[0] + ")";
}

bool MaxSignedCriterion::evaluate(const WorkspaceDict& wspc) {
    val = -1.0e20;
    // find the maximum value of data var1 with sign given by data var2
    const std::vector<scalar_type>& var1ref = wspc.dict.at(varnames[0]);
    const std::vector<scalar_type>& var2ref = wspc.dict.at(varnames[1]);
    for (index_type i = 0; i < var1ref.size(); ++i) {
        if ( var1ref[i] * std::copysign(1.0, var2ref[i]) > val) {
            wspcIndex = i;
            val = var1ref[i] * std::copysign(1.0, var2ref[i]);
        }
    }
    return (val > threshold);
}

std::string MaxSignedCriterion::description() const {
    return "max(" + varnames[0] + ")*sign(" + varnames[1] + ")";
}

bool MaxMagnitude2DCriterion::evaluate(const WorkspaceDict& wspc) {
    val = -1.0e20;
    const std::vector<scalar_type>& var1ref = wspc.dict.at(varnames[0]);
    const std::vector<scalar_type>& var2ref = wspc.dict.at(varnames[1]);
    for (index_type i = 0; i < var1ref.size(); ++i) {
        const scalar_type mag = std::sqrt(var1ref[i] * var1ref[i] + var2ref[i] * var2ref[i]);
        if (mag > val) {
            val = mag;
            wspcIndex = i;
        }
    }
    return (val > threshold);
}

std::string MaxMagnitude2DCriterion::description() const {
    return "max(sqrt(" + varnames[0] + "^2+" + varnames[1] + "^2))";
}

bool MaxMagnitude3DCriterion::evaluate(const WorkspaceDict& wspc) {
    val = -1.0e20;
    const std::vector<scalar_type>& var1ref = wspc.dict.at(varnames[0]);
    const std::vector<scalar_type>& var2ref = wspc.dict.at(varnames[1]);
    const std::vector<scalar_type>& var3ref = wspc.dict.at(varnames[2]);
    for (index_type i = 0; i < var1ref.size(); ++i) {
        const scalar_type mag = std::sqrt(var1ref[i] * var1ref[i] + var2ref[i] * var2ref[i] + var3ref[i] * var3ref[i]);
        if (mag > val) {
            val = mag;
            wspcIndex = i;
        }
    }
    return (val > threshold);
}

std::string MaxMagnitude3DCriterion::description() const {
    return "max(sqrt(" + varnames[0] + "^2+" + varnames[1] + "^2+" + varnames[2] + "^2))";
}

bool MaxAverageCriterion::evaluate(const WorkspaceDict& wspc) {
    scalar_type avg = 0.0;
    const std::vector<scalar_type>& varref = wspc.dict.at(varnames[0]);
    for (int i = 0; i < varref.size(); ++i)
        avg += varref[i];
    avg /= varref.size();
    val = avg;
    return (val > threshold);
}

std::string MaxAverageCriterion::description() const {
    return "max(avg(" + varnames[0] + "))";
}

bool MaxVariationCriterion::evaluate(const WorkspaceDict& wspc) {
    scalar_type avg = 0.0;
    scalar_type maxval = -1.0e20;
    const std::vector<scalar_type>& vec = wspc.dict.at(varnames[0]);
    for (int i = 0; i < vec.size(); ++i) {
        avg += vec[i];
        if ( vec[i] > maxval ) {
            maxval = vec[i];
            wspcIndex = i;
        }
    }
    avg /= vec.size();
    val = maxval - avg; 
    return (val > threshold);
}

std::string MaxVariationCriterion::description() const {
    return "max(max(" + varnames[0] + ")-avg(" + varnames[0] + "))";
}

}
