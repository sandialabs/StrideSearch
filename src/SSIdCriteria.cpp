#include "SSIdCriteria.hpp"
#include <algorithm>
#include <numeric>
#include <limits>
#include <cmath>

namespace StrideSearch {

bool MinCriterion::evaluate(const Workspace& wspc) {
    const RealArray& data = wspc.getConstDataRef(varnames[0]);
    auto minpos = std::min_element(data.begin(), data.end());
    val = *minpos;
    wspcIndex = minpos - data.begin();
    return val < threshold;
}
    
std::string MinCriterion::description() const {
    return "min(" + varnames[0] + ")";
}

bool MaxCriterion::evaluate(const Workspace& wspc) {
    auto data = wspc.getConstDataRef(varnames[0]);
    auto maxpos = std::max_element(data.begin(), data.end());
    val = *maxpos;
    wspcIndex = maxpos - data.begin();
    return val > threshold;
}

std::string MaxCriterion::description() const {
    return "max(" + varnames[0] + ")";
}

bool MaxSignedCriterion::evaluate(const Workspace& wspc) {
    auto vardata = wspc.getConstDataRef(varnames[0]);
    auto signdata = wspc.getConstDataRef(varnames[1]);
    val = 0.0;
    for (Int i=0; i<wspc.n; ++i) {
        const Real tval = std::copysign(1.0,signdata[i]) * vardata[i];
        if (tval > val) {
            wspcIndex = i;
            val = tval;
        }
    }
    return val > threshold;
}

std::string MaxSignedCriterion::description() const {
    return "max(sgn(" + varnames[1] + ")*" + varnames[0] + ")";
}

bool MaxMagnitudeCriterion::evaluate(const Workspace& wspc) {
    val = 0.0;
    switch (varnames.size()) {
        case (2) : {
            auto comp1 = wspc.getConstDataRef(varnames[0]);
            auto comp2 = wspc.getConstDataRef(varnames[1]);
            for (Int i=0; i<wspc.n; ++i) {
                const Real mag = std::sqrt(comp1[i]*comp1[i] + comp2[i]*comp2[i]);
                if (mag > val) {
                    wspcIndex = i;
                    val = mag;
                }
            }
            break;
        }
        case (3)  : {
            auto comp1 = wspc.getConstDataRef(varnames[0]);
            auto comp2 = wspc.getConstDataRef(varnames[1]);
            auto comp3 = wspc.getConstDataRef(varnames[2]);
            for (Int i=0; i<wspc.n; ++i) {
                const Real mag = std::sqrt(comp1[i]*comp1[i] + comp2[i]*comp2[i] + comp3[i]*comp3[i]);
                if (mag > val) {
                    wspcIndex = i;
                    val = mag;
                }
            }
            break;
        }
    }
    return val > threshold;
}

std::string MaxMagnitudeCriterion::description() const {
    std::string result("max(sqrt(");
    for (int i=0; i<varnames.size(); ++i) {
        result += varnames[i] + (i<varnames.size()-1 ? "^2+" : "^2))");
    }   
    return result;
}

bool MaxAverageCriterion::evaluate(const Workspace& wspc) {
    switch (wspc.nVars()) {
        case (1) : {
            auto vardata = wspc.getConstDataRef(varnames[0]);
            val = std::accumulate(vardata.cbegin(), vardata.cend(), 0.0)/wspc.n;
            break;
        }
        case (2) : {
            auto vardata = wspc.getConstDataRef(varnames[0]);
            auto weight = wspc.getConstDataRef(varnames[1]);
            val = 0.0;
            Real area = 0.0;
            for (int i=0; i<wspc.n; ++i) {
                val += vardata[i]*weight[i];
                area += weight[i];
            }
            val /= area;
            break;
        }
    }
    return val > threshold;
}

std::string MaxAverageCriterion::description() const {
    return "max(avg(" + varnames[0] + "))";
}

bool MaxVariationCriterion::evaluate(const Workspace& wspc) {
    auto vardata = wspc.getConstDataRef(varnames[0]);
    const Real avg = std::accumulate(vardata.cbegin(), vardata.cend(), 0.0)/wspc.n;
    auto maxpos = std::max_element(vardata.cbegin(), vardata.cend());
    val = *maxpos - avg;
    wspcIndex = maxpos - vardata.cbegin();
    return val > threshold;
}

std::string MaxVariationCriterion::description() const {
    return "max(max(" + varnames[0] + ")-avg(" + varnames[0] + "))";
}


bool MaxVariationOfAverageCriterion::evaluate(const Workspace& wspc) {
    auto var1 = wspc.getConstDataRef(varnames[0]);
    auto var2 = wspc.getConstDataRef(varnames[1]);
    Real avg2 = 0;
    Real max1 = std::numeric_limits<Real>::lowest();
    for (Int i=0; i<wspc.n; ++i) {
        const Real avg1 = 0.5*(var1[i]+var2[i]);
        avg2 += avg1;
        if (avg1 > max1) {
            max1 = avg1;
            wspcIndex = i;
        }
    }
    avg2 /= wspc.n;
    val = max1-avg2;
    return val > threshold;
}

std::string MaxVariationOfAverageCriterion::description() const {
    const std::string avg1str = "avg(" + varnames[0] + "+" + varnames[1] + ")";
    return "max(max(" + avg1str + ")-avg("+ avg1str + "))";
}

}
