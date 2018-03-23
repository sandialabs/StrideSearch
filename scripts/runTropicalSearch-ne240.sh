#!/bin/bash

dataDir=/Volumes/Storage/polarStorms/ne240
outputLoc=$HOME/Desktop/ssearchResults
exeLoc=$HOME/Desktop/StrideSearch/install/bin

cp $exeLoc/tropicalSearch.exe $dataDir/.

cd $dataDir

for filename in *.nc
do 
cat <<EOF > tropicalSearch-ne240.namelist
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
	outputRoot = '${filename%.nc}-timing'
	doThickness = .FALSE.
/
EOF

./tropicalSearch.exe tropicalSearch-ne240.namelist

cat $outputLoc/${filename%.nc}-timing.txt >> $outputLoc/sSearchOutputFile-ne240-timing.txt

done