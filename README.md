# StrideSearch
Storm detection in climate data

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains certain rights in this software.

Algorithms to detect and track storms from model output or reanalysis data.

Storm detection and tracking algorithm
=========
This library provides methods for searching climate data sets 
in [NetCDF](http://www.unidata.ucar.edu/software/netcdf/) format for various kinds of storms which may be defined by the user. @n
Storm identification criteria may correspond to any meteorological phenomena of interest that can be described quantitatively
in terms of available data. 

The algorithm reads the input data file and at each time step completes a spatial search, outputing locations that meet
or exceed the spatial identification criteria.@n
Following the spatial search, detected storms are built into tracks using a temporal correlation program, which also applies
temporal identification criteria.

Spatial search
---------------
The user must specify a sector radius (in km) to define the search sectors.  @n
Only one storm may be detected per radius so this input is a tunable parameter of the algorithm.  @n
Depending on the application, larger or smaller radii may be desired.  @n
Any 2 storms that are separated by a great-circle distance less than the sector radius will be considered duplicates and one of them will be skipped.

The spatial search algorithm:
* Covers the requested search area (specified by a maximum and minimum latitude) with overlapping circular search sectors, 
defined by the user-specified great-circle distance in km.	  
* Reads the NetCDF input file for relevant storm data (i.e., windspeed, sea level pressure, vorticity) in each storm sector.
* Stores information about any sector (and its relevant data) in a local data structure for thread-parallelism.   
* Outputs the set of found storms to a data file for later input to the track-building algorithm.

The main class responsible for accomplishing the spatial search is StrideSearch::SearchManager.
It requires the user provide StrideSearch::IDCriterion derived classes and StrideSearch::CollocationCriterion pointers to define (quantitatively) the features they seek from their data.

Upon completing its search, StrideSearch::SearchManager contains a StrideSearch::EventSet containing the records of all events found from the data.  Each StrideSearch::Event contains all information required to locate the corresponding feature in the data set, including filename, location index, time index, etc.

Temporal correlation
-----------------
The temporal correlation program takes the output of all per-timestep Spatial Searches and examines the data for correlations
across adjacent time steps to build storm tracks. @n
Users must define a maximum storm speed (m/s).  @n
This is used to determine the maximum distance a storm can travel per timestep. 

* Beginning at the first timestep containing detected storms in the output of the spatial search procedure, 
the tracking program starts with the first detected storm and searches subsequent timesteps for nearest neighbors.
* Successive locations are stored in a possible track.
* Temporal identification criteria (e.g., minimum duration, distance traveled over time, etc.) are applied to each possible track.
* If the temporal criteria are met or exceeded, the possible track is saved to output.  
* The search begins again with the next storm detection from the spatial search output, and continues until all data
has been searched.

Software requirements
===============
The software requires [NetCDF](http://www.unidata.ucar.edu/software/netcdf/) and either [NetCDF C++](http://www.unidata.ucar.edu/software/netcdf/)
or libraries, as well as [CMake](https://cmake.org) for its configuration step.
Either a C++ compiler (preferred) or a modern Fortran compiler are also required.      @n
An MPI distribution is not required.   @n   


Build / Install
===============
From the base StrideSearch directory, create a build directory:
    mkdir build
    cd build

Configure with CMake using a configure script similar to the one below; save it in the build directory as `configureStrideSearch.sh`.

    #!/bin/bash
    
    export NETCDF_ROOT=/opt/netcdf-4.4.1
    SRC_ROOT=$HOME/StrideSearch/
    
    rm -f CMakeCache.txt
    rm -rf CMakeFiles/
    
    EXTRA_ARGS=$1
    
    cmake \
    -D CMAKE_BUILD_TYPE:STRING="RELEASE" \
    -D CMAKE_VERBOSE_MAKEFILE:BOOL=OFF \
    $EXTRA_ARGS \
    $SRC_ROOT


Then, build the project by typing 

    ./configureStrideSearch.sh
    make -j 4 && make install
    make test

If Doxygen documentation is desired, type 

    make docs

Stride Search Conventions
===========

Indices
-------
- 2d indices (i,j): i = latitude index, j = longitude index
- 2d coordinates are (lat,lon), i.e., (lats[i], lons[j])

Units
------
- Spatial units are kilometers
- Angular units are degrees
- Stride Search time units are hours, and thus integers
- NetCDF time units are days, and may have fractional values; they are floating point types

DataLayout
---------
Fundamental template parameter@n
- Run the same test on different data layouts with a simple typedef


To-do
=========
[ ] Doxygen class design plans & responsibilities@n
[ ] Vorticity, gradient, etc., computation using gmls@n
[ ] EventSet needs output capabilities for python, ncl, text, etc.@n
[ ] MPI parallel over files @n
[ ] MPI rank-guard std::cout, etc., in SearchManager@n
[ ] Thread parallel over sectors @n
[ ] Generalize time for various units @n
[ ] Track utilities @n
[ ] Generate a small test data set for TC spatial search@n
[ ] Replace DateTime with ctime equivalents@n
[ ] Update main doxygen start page to cpp version@n

References
=================
Stride Search is described in more detail in P. Bosler et al., 2015, _Geosci. Model Dev._.  

Included in the Stride Search software are two examples of driver programs.  @n
1. The tropical cyclone driver uses identification criteria defined in F. Vitart et al., 1997, _J. Climate_, 10:745-760.
2. The polar low search (Fortran only) uses the identification criteria given by T. J. Bracegirdle and S. L. Gray, 2008, _Int. J. Climatology_, 28:1903-1919.


