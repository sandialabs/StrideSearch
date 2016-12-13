"""
This module defines the abstract base class for Event Identification Criteria in Stride Search.
Implementations for basic criteria are included.
"""
from Event import print_copyright
from abc import ABCMeta, abstractmethod
from numpy import amax, amin, ones, mean

class Criterion(object):
    __metaclass__ = ABCMeta
    
    def __init__(self, varname, threshold):
        self.varname = varname
        self.threshold = threshold
        #if crit_type not in criteria_types:
        #    raise NotImplementedError("Criterion ERROR: requested criteria (%s) not implemented."%crit_type)
        
    def __repr__(self):
        return "<%s: varname = %s, threshold = %s>"%(self.__class__.__name__, self.varname, self.threshold)
    
    @abstractmethod        
    def evaluate(self):
        pass
        
class MaxCriterion(Criterion):
    """ 
    Maximum value criterion.  Returns True if the maximum value of the data 
    meets or exceeds the threshold
    """
    def evaluate(self, data):
        if amax(data.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False
                
class MinCriterion(Criterion):
    """ 
    Minimum value criterion.  Returns True if the minimum value of the data meets 
    or falls below the threshold.
    """
    def evaluate(self, data):
        if amin(data.ravel(), axis=0) < self.threshold:
            return True
        else:
            return False        

class MaxAverageCriterion(Criterion):
    """
    Maximum average criterion. Returns True if the mean of the data meets or 
    exceeds the threshold.
    """ 
    def evaluate(self, data):
        if mean(data.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False

class VariationExcessCriterion(Criterion):
    """
    Variation excess criterion.  Returns True if the maximum data value exceeds 
    the data average by the threshold value or greater.
    """                        
    def evaluate(self, data):
        if amax(data.ravel(), axis=0) - mean(data.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False

class DifferenceCriterion(Criterion):
    """
    Difference criterion. Returns True if the maximum element-wise value of the 
    difference data1 - data2 meets or exceeds the threshold.
    """
    def evaluate(self, data1, data2):
        if amax(data1.ravel() - data2.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False
            
            
        
if __name__ == "__main__":
    print_copyright()
    c = MaxCriterion("vorticity", 1.0E-4)
    vor_data = ones((2,5))
    print(c)
    print "criterion eval: ", c.evaluate(vor_data)
    aa = 2.0 * ones(4)
    bb = 6.0 * ones(4)
    cd = DifferenceCriterion("b-a", 1.0)
    print cd
    print "criterion eval: ", cd.evaluate(bb,aa)
        
                        