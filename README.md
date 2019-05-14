Stride Search Conventions
===========

Indices
-------
- 2d indices (i,j): i = latitude index, j = longitude index
- 2d coordinates are (lat,lon), i.e., (lat[i], lon[j])

Units
------
- Spatial units are kilometers
- Angular units are degrees
- Time units are hours

DataLayout
---------
Fundamental template parameter@n
- Run the same test on different data layouts with a simple typedef


To-do
=========
[x] #ifdef kokkos for real array type@n
[x] Reorg. repo so cxx is prominent@n
[ ] Doxygen class design plans & responsibilities@n
[ ] Vorticity computation using gmls@n
[x] Intensity traits and locality traits for idcriteria@n
[ ] Consistent template DataLayout throughout@n
[ ] Decide whether to keep NCReader or consolidate its functions into SSData@n

