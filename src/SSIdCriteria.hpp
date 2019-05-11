#ifndef _STRIDE_SEARCH_ID_CRITERION_BASE_H_
#define _STRIDE_SEARCH_ID_CRITERION_BASE_H_

#include "StrideSearchConfig.h"
#include "SSDefs.hpp"
#include "SSConsts.hpp"
#include "SSUtilities.hpp"
#include "SSEventTraits.hpp"
#include "SSWorkspace.hpp"
#include <string>
#include <vector>

namespace StrideSearch {

/// Base class and interface for all identification criteria.
/**
    Each subclass must relate to a single, quantifiable identification criterion for a particular storm.
    
    The IDCriterion class is one of the fundamental StrideSearch classes (along with Sector and Event).
*/
class IDCriterion {
    protected:
        /// General constructor.
        /**
            @param vars Names of variables in data file related to this criterion.
            @param thresh Threshold value
            @param cmprKind Comparison type to determine whether a value must exceed the threshold or fall beneath 
            @param locKind Location type: independent or not.
        */
        IDCriterion(const std::vector<std::string>& vars, const Real thresh, 
            const IntensityComparison cmprKind, const SpatialDependence locKind) :
                varnames(vars), threshold(thresh), compareKind(cmprKind), locationKind(locKind),
                val(0), wspcIndex(-1) {}
        
        /// Specialized constructor for the case of 1 variable.
        IDCriterion(const std::string& var, const Real thresh, 
            const IntensityComparison cmprKind, const SpatialDependence locKind) : varnames(1,var),
                threshold(thresh), compareKind(cmprKind), locationKind(locKind), val(0), wspcIndex(-1) {}

    public:
        /// Event intensity comparison type
        IntensityComparison compareKind;
        
        /// Event location dependence type
        SpatialDependence locationKind;
        
        /// Variable names associated with this criterion
        std::vector<std::string> varnames;
        /// Threshold value for this criterion
        Real threshold;
        /// Index of a triggered Event within a Sector's workspace
        Index wspcIndex;
        /// Value of a triggered Event.  This is only defined if the Criterion has evaluated True.
        Real val;
        
        virtual ~IDCriterion(){};
        
        /// Each subclass defines this method that has 2 responsibilities:
        /** 1. It computes the value of its associated data (a reduction).
            2. It compares that value against its threshold.
        */   
        virtual bool evaluate(const Workspace& wspc) = 0;
        
        /// Required by all subclasses: a unique description of the Event triggered by this criterion evaluating true.
        /**
            Each description must include the variable names used to evaluate the criterion.
        */
        virtual std::string description() const = 0;
};

/// Compares the data minimum to the threshold
/** 
    Used e.g., for surface pressure.
*/
class MinCriterion : public IDCriterion {
    public:
        MinCriterion(const std::string& var, const Real thresh) : 
            IDCriterion(var, thresh, IntensityComparison::LESS_THAN, SpatialDependence::INDEPENDENT) {}
    
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

/// Compares data maximum to threshold
/** 
    Used e.g., for wind speed
*/
class MaxCriterion : public IDCriterion {
    public:
        MaxCriterion(const std::string& var, const Real thresh) :
            IDCriterion(var, thresh, IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT) {}
        
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

/// Compares the maximum of a variable multiplied by the sign of another variable against threshold.
/**
    Used e.g., for cyclonic vorticity
*/
class MaxSignedCriterion : public IDCriterion {
    public:
        MaxSignedCriterion(const std::string& var, const std::string& signvar, const Real thresh ) :
            IDCriterion(std::vector<std::string>({var, signvar}), thresh, IntensityComparison::GREATER_THAN,
                SpatialDependence::INDEPENDENT) {}
        
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

/// Compares the maximum of a vector's magnitude against threshold
/** used for windspeed, for example.
*/
class MaxMagnitudeCriterion : public IDCriterion {
    public:
        MaxMagnitudeCriterion(const std::string& comp1var, const std::string& comp2var, const Real thresh) :
            IDCriterion(std::vector<std::string>({comp1var, comp2var}), thresh, IntensityComparison::GREATER_THAN,
                SpatialDependence::INDEPENDENT) {}
        
        MaxMagnitudeCriterion(const std::string& comp1var, const std::string& comp2var, const std::string comp3var,
            const Real thresh) : IDCriterion(std::vector<std::string>({comp1var, comp2var, comp3var}),
                thresh, IntensityComparison::GREATER_THAN, SpatialDependence::INDEPENDENT) {}
        
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

class MaxAverageCriterion : public IDCriterion {
    public:
        MaxAverageCriterion(const std::string& var, const Real thresh) :
            IDCriterion(var, thresh, IntensityComparison::GREATER_THAN, SpatialDependence::DEPENDENT) {}
            
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

class MaxVariationCriterion : public IDCriterion {
    public:
        MaxVariationCriterion(const std::string& var, const Real thresh) :
            IDCriterion(var, thresh, IntensityComparison::GREATER_THAN, SpatialDependence::DEPENDENT) {}
        
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

class MaxVariationOfAverageCriterion : public IDCriterion {
    public:
        MaxVariationOfAverageCriterion(const std::string& var1, const std::string& var2, const Real thresh) :
            IDCriterion(std::vector<std::string>({var1, var2}), thresh, IntensityComparison::GREATER_THAN,
                SpatialDependence::DEPENDENT) {}
        
        bool evaluate(const Workspace& wspc);
        
        std::string description() const;
};

}
#endif
