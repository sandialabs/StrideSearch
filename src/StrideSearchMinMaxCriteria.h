#ifndef _STRIDE_SEARCH_MIN_MAX_CRITERIA_H_
#define _STRIDE_SEARCH_MIN_MAX_CRITERIA_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchIDCriterionBase.h"
#include "StrideSearchWorkspaceDict.h"

namespace StrideSearch {

/// Compares the minimum data value in the workspace to the threshold.
class MinCriterion : public IDCriterion {
    public:
    MinCriterion(const std::string varname, const scalar_type threshold) : 
        IDCriterion(varname, threshold, Event::LESS_THAN) {};
    
    /// Returns true if a value in a Sector falls below threshold.
    bool evaluate(const WorkspaceDict& wspc) ;
    std::string description() const;
};

/// Compares the maximum data value in the workspace to the threshold.
class MaxCriterion : public IDCriterion {
    public:
    MaxCriterion(const std::string varname, const scalar_type threshold) : 
        IDCriterion(varname, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if a value in a Sector exceeds threshold.
    bool evaluate(const WorkspaceDict& wspc) ;
    std::string description() const;
};

/// Compares the maximum value of the product varname * sign(signvarname) to the threshold.
/**
    This is especially useful with vorticity and latitude, since cyclonic vorticity has opposite signs in the 
    northern and southern hemisphere.
*/
class MaxSignedCriterion : public IDCriterion {
    public:
    MaxSignedCriterion(const std::string varname, const std::string signVarname, const scalar_type threshold) : 
        IDCriterion(varname, signVarname, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if a value of sign(signVarname) * varname exceeds threshold
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

/// Compares the maximum magnitude of a 2-component vector to the threshold.
/**
    This is used, for example, to find maximum horizontal wind speeds given in zonal and meridional components.
*/
class MaxMagnitude2DCriterion : public IDCriterion {
    public:
    MaxMagnitude2DCriterion(const std::string comp1var, const std::string comp2var, const scalar_type threshold) :
        IDCriterion(comp1var, comp2var, threshold, Event::GREATER_THAN) {};
    MaxMagnitude2DCriterion(const std::vector<std::string>& vecComponentVarnames, const scalar_type threshold) : 
        IDCriterion(vecComponentVarnames, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if sqrt(varnames[0]^2 + varnames[1]^2) exceeds threshold
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

/// Compares the maximum magnitude of a 3-component vector to the threshold.
/**
    This is used, for example, to find maximum wind speeds given a three-component wind vector.
*/

class MaxMagnitude3DCriterion : public IDCriterion {
    public:
    MaxMagnitude3DCriterion(const std::vector<std::string>& vecComponentVarnames, const scalar_type threshold) : 
        IDCriterion(vecComponentVarnames, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if sqrt(vecComponentVarnames[0]^2 + vecComponentVarnames[1]^2 + vecComponentVarnames[2]^2) exceeds threshold.
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

/// Compares the arithmetic average of a variable to the threshold.
/**
 @todo Add capability for spatial averages
*/
class MaxAverageCriterion : public IDCriterion {
    public:
    MaxAverageCriterion(const std::string varname, const scalar_type threshold) : 
        IDCriterion(varname, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if a Sector's average data value exceeds threshold.
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

/// Compares the difference between a sector's maximum value and the sector average for the same variable.
class MaxVariationCriterion : public IDCriterion {
    public:
    MaxVariationCriterion(const std::string varname, const scalar_type threshold) :
        IDCriterion(varname, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if the difference between a Sector's maximum data value the Sector average exceeds threshold.
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

/// Compares the difference between a sector's maximum value and the sector average for a variable's vertical average
/**
    Used, for example, as a warm-core identification criteria (Vitart et. al., 1997) for tropical cyclones.
*/
class MaxVariationCriterionVerticalAvg : public IDCriterion {
    public:
    MaxVariationCriterionVerticalAvg(const std::string varname_low_level, 
        const std::string varname_hi_level, const scalar_type threshold) : 
        IDCriterion(varname_low_level, varname_hi_level, threshold, Event::GREATER_THAN) {};
    
    /// Returns true if the difference between the maximum value of the average of var1 and var2 and the average value of the average of var1 and var2 exceeds threshold.
    bool evaluate(const WorkspaceDict& wspc);
    std::string description() const;
};

}
#endif