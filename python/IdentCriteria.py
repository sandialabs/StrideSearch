"""
This module defines the abstract base class for Event Identification Criteria in Stride Search.
Implementations for basic criteria are included.
"""
from Event import Event, print_copyright
from abc import ABCMeta, abstractmethod
from numpy import amax, amin, ones, mean, argmax, argmin
from SectorList import Sector
from datetime import datetime

class Criterion(object):
"""
Each criterion will be evaluated once for each sector, during each time step.
If an evaluation returns True, then the Criterion can return the corresponding Event.
"""
    __metaclass__ = ABCMeta
    
    def __init__(self, varname, threshold):
        self.varnames = [varname]
        self.threshold = threshold
        #if crit_type not in criteria_types:
        #    raise NotImplementedError("Criterion ERROR: requested criteria (%s) not implemented."%crit_type)
    
    def addVariableName(self, varname):
        self.varnames.append(varname)    
                
    def __repr__(self):
        return "<%s: varname = %s, threshold = %s>"%(self.__class__.__name__, self.varnames[0], self.threshold)
    
    @abstractmethod        
    def evaluate(self, sector, workspace):
        pass
      
    @abstractmethod
    def returnEvent(self, data, sector, workspace):
        pass
        
class MaxCriterion(Criterion):
    """ 
    Maximum value criterion.  Returns True if the maximum value of the data 
    meets or exceeds the threshold
    """
    def evaluate(self, sector, workspace):
        if amax(workspace[self.varnames[0]], axis=0) >= self.threshold:
            return True
        else:
            return False
            
    def returnEvent(self, sector, workspace, dtime):
    	val = amax(workspace[self.varnames[0]])
        ind = argmax(workspace[self.varnames[0]])
        vals = {varnames[0] : maxval}
        desc = varnames[0] + " max"
        return Event(desc, sector.dataPoints[ind], dtime, sector.dataPointIndices[ind], vals)

                                                        
class MinCriterion(Criterion):
    """ 
    Minimum value criterion.  Returns True if the minimum value of the data meets 
    or falls below the threshold.
    """
    def evaluate(self, sector, workspace):
        if amin(data.ravel(), axis=0) < self.threshold:
            return True
        else:
            return False
            
    def returnEvent(self, sector, workspace):
        pass
 

class MaxAverageCriterion(Criterion):
    """
    Maximum average criterion. Returns True if the mean of the data meets or 
    exceeds the threshold.
    
    NOTE: The average computed here is an arithmetic average, not a spatial average.
    """ 
    def evaluate(self, sector, workspace):
        if mean(data.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False
    def returnEvent(self, sector, workspace):
        pass
  

class VariationExcessCriterion(Criterion):
    """
    Variation excess criterion.  Returns True if the maximum data value exceeds 
    the data average by the threshold value or greater.
    
    NOTE: The average computed here is an arithmetic average, not a spatial average.
    """                        
    def evaluate(self, sector, workspace):
        if amax(data.ravel(), axis=0) - mean(data.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False
    def returnEvent(self, sector, workspace):
        pass
        
   

class DifferenceCriterion(Criterion):
    def __init__(self, varname1, varname2, threshold):
        Criterion.__init__(self, varname1, threshold)
        self.addVariableName(varname2)
    """
    Difference criterion. Returns True if the maximum element-wise value of the 
    difference data1 - data2 meets or exceeds the threshold.
    """
    def evaluate(self, sector, workspace):
        if amax(data1.ravel() - data2.ravel(), axis=0) >= self.threshold:
            return True
        else:
            return False
            
    def __repr__(self):
       return "<%s: varname = %s, varname2 = %s, threshold = %s>"%(self.__class__.__name__, 
        self.varnames[0], self.varnames[1], self.threshold)       
    
    def returnEvent(self, sector, workspace):
        pass 
        
                   
        
if __name__ == "__main__":
    print_copyright()
    c = MaxCriterion("vorticity", 1.0E-4)
    vor_data = ones((2,5))
    print(c)
    #print "criterion eval: ", c.evaluate(vor_data)
    aa = 2.0 * ones(4)
    bb = 6.0 * ones(4)
    cd = DifferenceCriterion("b","a", 1.0)
    print cd
    #print "criterion eval: ", cd.evaluate(bb,aa)
        
                        