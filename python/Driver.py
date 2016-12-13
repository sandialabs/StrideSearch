"""
Stride Search driver.

Copyright 2016 Sandia Corporation. Under the terms of Contract DE-AC04-94AL85000
with Sandia Corporation, the U.S. Government retains certain rights in this
software.
"""
import glob
import os
import netCDF4 as nc


#
#   USER: DEFINE PATH TO DATA
#
dataPath = "/Users/pabosle/Desktop/dataTemp"

#
#   USER: DEFINE IDENTIFICATION CRITERIA
#
criteria = []


def search(sectors, criteria, L):
    pass

#
#   Get list of all netCDF files to search
#
os.chdir(dataPath)
ncFiles = glob.glob("*.nc")

#
#   Build search sectors
#

# start with empty event list
L = []
#
#   Loop over files
#
for dfile in ncFiles:
    dataFile = nc.Dataset(dfile, "r")
    filetime = dataFile.variables["time"]
    #
    #   Loop over time steps
    #
    for time_ind in range(len(filetime)):
        pass # do stride search (search by sector, append events to L)


