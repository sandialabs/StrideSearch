# StrideSearch
Storm detection in climate data

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains certain rights in this software.

Algorithms to detect and track storms from model output or reanalysis data.

NEW!
=======
Python implementation in `StrideSearch/python`.  Many more capabilities than the Fortran implementation, but possibly slower.  
Development is ongoing.

Storm detection and tracking algorithm
=========
This library provides methods for searching latitude-longitude data sets 
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
Any 2 storms that are separated by a great-circle distance less than the sector radius will be considered duplicates and one of them will be removed.

The spatial search algorithm:
* Covers the requested search area (specified by a maximum and minimum latitude) with overlapping circular search sectors, 
defined by the user-specified great-circle distance in km.	  
* Reads the NetCDF input file for relevant storm data (i.e., windspeed, sea level pressure, vorticity) in each storm sector.
* Stores information about any sector (and its relevant data) in a linked-list data structure if that sector's data
exceeds some user-defined storm threshold.   
* After all sectors have been searched, it compares the found storms from the linked-list to remove duplicates.  
* Outputs the linked-list to various formats.

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
The software requires [NetCDF](http://www.unidata.ucar.edu/software/netcdf/) and [NetCDF Fortran](http://www.unidata.ucar.edu/software/netcdf/)
libraries and a modern Fortran compiler.      @n
An MPI distribution is not required.   @n   

NetCDF file access utilities provided by the __Geophysical Fluid Dynamics Laboratory's (GFDL)__ 
[TSTORMS](http://www.gfdl.noaa.gov/tstorms) code for tropical cyclone detection are incorporated into this software.

Build / Install
===============
This version has been updated to use the CMake cross-platform Makefile generator.  A sample configure script is

    #!/bin/bash

    export CC=mpicc
    export CXX=mpicxx
	export FC=mpifort
	export NETCDF=/opt/netcdf-4.3.2
	
	SRC_DIR=$HOME/StrideSearch
	BUILD_DIR=$SRC_DIR/build
	INSTALL_DIR=$SRC_DIR/install

	rm -rf CMakeCache.txt
	rm -rf CMakeFiles/

	cmake \
	-D CMAKE_BUILD_TYPE:STRING=RELEASE \
	-D CMAKE_INSTALL_PREFIX:FILEPATH=$INSTALL_DIR \
	$SRC_DIR

Users should edit the `-D CMAKE_INSTALL_PREFIX:FILEPATH` variable to their desired install directory, and edit the 
`NETCDF` variable to the root of their local [NetCDF](http://www.unidata.ucar.edu/software/netcdf/) library.  @n
Note: the [NetCDF](http://www.unidata.ucar.edu/software/netcdf/) Fortran library must be built
separately from the NetCDF C library.  @n
This build script assumes they are installed to the same location.

After running the configure script, build the project with 

    make
    
Then install it with 

	make install

Software / algorithm design
===============
The fundamental data structure in the StrideSearch software is a linked-list e.g. @ref StormListNode, @ref TrackListNode.  @n
We do not know in advance how many storms will be detected per timestep, or how many storm tracks may be built from one dataset. @n
Linked-lists handled these unknowns using dynamic memory allocation; each time a new storm or new track is found, the software
allocates memory for that storm and saves its information to that newly allocated memory.  @n
The basic structure of the linked-lists used by StrideSearch is 

    type ListNode
    	real :: aStormDatum
    	integer :: anotherStormDatum
    	
    	type(ListNode), pointer :: nextNode
    end type
    
Users may alter the data items of a list node (StrideSearch has an example of this: compare @ref StormListNode to @ref TropicalStormListNode). @n
The pointer `nextNode` is the means of constructing and traversing a list.  @n
It points to the location in memory of the next node or, if it is the end of the list, it is a null pointer.  @n
Both the spatial search algorithm (@ref StrideSearch ) and the temporal correlation algorithm (@ref TrackListNode) use this basic data structure.

Lists are traversed with other pointers. @n
A pointer, say `current`, may point to the current node and be used to access its data.@n
Then the entire list may be traversed with code similar to the following

    type(ListNode), pointer :: current

    current => listRoot
    do while ( associated(current) )
    	! < do something with the data at current node >
    	current => current%nextNode
    enddo
    
This do loop will automatically terminate when `current` reaches the null pointer `current%%nextNode` at the end of the list.

This basic data structure is applied to each application.  Each application has three files (and three data types) associated with it.
1. A data file.  StrideSearchData provides the basic data container for netcdf data to be searched.   This data type may
be extended to facilitate other applications.  Examples are provided by TropicalData and PolarData.
2. A storm list node. The linked list of storms per-timestep that will be output by the search algorithm.  The basic type is 
given by StormListNode, and example extensions may be found in TropicalStormListNode and PolarLowListNode.
3. A driver program.  Examples are given by PolarSearch.f90 and TropicalSearchDriver.f90.

Use
===============
Users may extend the basic @ref StormListNode data type following the example of @ref TropicalStormListNode to an application of their choosing. @n
Each linked list relies on the `initialize` and `Copy` subroutines to construct linked lists, and must also implement 
`removeNodeFromList` and recursive `deleteList` procedures.  @n
The @ref StrideSearchData class and subroutines should also be extended or altered to read the data corresponding to the new application. @n
Finally, the @ref stridesearchmodule::dostridesearch subroutine should be modified to include the storm identification criteria suitable for the new application. @n
An example of each of these extensions of the basic Stride Search data types and methods are provided by @ref TropicalData and @ref TropicalStrideSearch.


References
=================
Stride Search is described in more detail in P. Bosler et al., 2015, _Geosci. Model Dev._.  

Stride Search uses the netcdf file access subprograms provided by GFDL's [TSTORMS](http://www.gfdl.noaa.gov/tstorms) software.
These are contained in the `<stride search root>/gfdlUtilities` subdirectory.  

Included in the Stride Search software are two examples of driver programs.  @n
1. The tropical cyclone driver uses identification criteria defined in F. Vitart et al., 1997, _J. Climate_, 10:745-760.
2. The polar low search uses the identification criteria given by T. J. Bracegirdle and S. L. Gray, 2008, _Int. J. Climatology_, 28:1903-1919.


