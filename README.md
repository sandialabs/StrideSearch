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
[ ] Thread parallel over sectors @n
[ ] Generalize time for various units @n
[x] SearchManager needs tests @n
[x] Tropical cyclone spatial search example @n
[ ] Track utilities @n
[x] Progress bar@n
[ ] Generate a small test data set for TC spatial search@n
[ ] DateTime doxygen: 0-indexing.
