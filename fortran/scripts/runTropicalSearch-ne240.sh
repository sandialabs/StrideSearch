#!/bin/bash

dataDir=/ascldap/users/mataylo/scratch1/acme_v1/latlon
outputLoc=$HOME/strideSearchResults
exeLoc=/ascldap/users/pabosle/StrideSearch/build

cp $exeLoc/tropicalSearch.exe $outputLoc/.

cd $dataDir

for filename in *.nc
do 
cat <<EOF > $outputLoc/tropicalSearch-ne120.namelist
&input
	ncfilename = '${dataDir}/${filename}'
	southernBoundary = -40.0
	northernBoundary = 40.0
	sectorRadius = 450.0
	pslThreshold = 1.0e20
	windThreshold = 0.0
	vortThreshold = 8.5e-4
	vortPslDistThreshold = 450.0
	tempPslDistThreshold = 225.0
	tempExcessThreshold = 2.0
	outputDir = '${outputLoc}'
	doThickness = .FALSE.
	outputRoot = '${filename%.nc}'
/
EOF

$outputLoc/tropicalSearch.exe $outputLoc/tropicalSearch-ne120.namelist

cat $outputLoc/${filename%.nc}.txt >> $outputLoc/sSearchOutputFile-ne120_8.5e-4_2.0C.txt

done