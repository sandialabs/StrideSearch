#!/bin/bash

dataDir=/Volumes/Storage/stormSearchData/latlon/ll91x180

outputLoc=/Volumes/Storage/tropicalStorms/quarterdeg

exeLoc=$HOME/Desktop/StrideSearch/install/bin

cp $exeLoc/tropicalSearch.exe $dataDir/.

cd $dataDir

for filename in *.nc
do 
cat <<EOF > tropicalSearch-timing.namelist
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
	outputRoot = '${filename%.nc}-quarterdeg-timing'
/
EOF

./tropicalSearch.exe tropicalSearch-timing.namelist

cat $outputLoc/${filename%.nc}-quarterdeg-timing.txt >> $outputLoc/sSearchOutputFile-quarterdeg-timing.txt

done

