#ifndef _STRIDE_SEARCH_ID_CRITERION_BASE_H_
#define _STRIDE_SEARCH_ID_CRITERION_BASE_H_

#include "StrideSearchConfig.h"
#include "StrideSearchTypeDefs.h"
#include "StrideSearchUtilities.h"
#include "StrideSearchEvent.h"
#include "StrideSearchWorkspaceDict.h"
#include <string>
#include <vector>

namespace StrideSearch {

/// Base class and interface for all identification criteria.
/**
    This class may be subclassed to define new applications.
    
    Each subclass must relate to a single, quantifiable identification criterion for a particular storm.
    Combinations of subclasses may be built (see Sector::evaluateCriteriaAtTimestep, for example) for more specific applications;
    see examples/SSTropicalCyclone.cpp for an example.
    
    The IDCriterion class is one of the fundamental StrideSearch classes (along with Sector and Event).
*/
class IDCriterion {
    protected:
        /// General constructor.
        /**
            @param vars Names of variables in data file related to this criterion.
            @param thresh Threshold value
            @param cmprKind Comparison type to determine whether a value must exceed the threshold or fall beneath the 
            threshold
        */
        IDCriterion(const std::vector<std::string>& vars, const scalar_type thresh, const Event::IntensityComparison cmprKind);
        
        /// Specialized version of general constructor for the case of 1 criterion requiring 1 variable.
        IDCriterion(const std::string& var, const scalar_type thresh, const Event::IntensityComparison cmprKind);
        /// Specialized version of general constructor for the case of 1 criterion requiring 2 variables.
        IDCriterion(const std::string& var1, const std::string& var2, const scalar_type thresh, 
            const Event::IntensityComparison cmprKind);

    public:
        /// Event intensity comparison type
        Event::IntensityComparison compareKind;
        /// Variable names associated with this criterion
        std::vector<std::string> varnames;
        /// Threshold value for this criterion
        scalar_type threshold;
        /// Index of a triggered Event within a Sector's workspace
        index_type wspcIndex;
        /// Value of a triggered Event.  This is only defined if the Criterion has evaluated True.
        scalar_type val;
        
        virtual ~IDCriterion(){};
           
        virtual bool evaluate(const WorkspaceDict& wspc) = 0;
        
        /// Required by all subclasses: a unique description of the Event triggered by this criterion evaluating true.
        /**
            Each description must include the variable names used to evaluate the criterion.
        */
        virtual std::string description() const = 0;
};


}
#endif
