#!/bin/bash

atmDataDir=$HOME/Desktop/dataTemp

exeDir=$HOME/StrideSearch/install/bin

outputLoc=$HOME/Desktop/dataTemp/ssResults
outputRoot=ne240-year4-summerTropicals

rm -f $outputLoc/$outputRoot.txt

cd $atmDataDir
cp $exeDir/tropicalSearch.exe .

for filename in *0004*.nc
do
cat <<EOF > tropSearch.namelist
&input
ncfilename = '${atmDataDir}/${filename}'
southernBoundary = -40.0
northernBoundary = 40.0
sectorRadius = 450.0
pslThreshold = 99000.0
windThreshold = 10.0
vortThreshold = 8.5e-4
vortPslDistThreshold = 450.0
tempPslDistThreshold = 225.0
tempExcessThreshold = 2.0
outputDir = '${outputLoc}'
outputRoot = '${outputRoot}'
/
EOF

./tropicalSearch.exe tropSearch.namelist

cat $outputLoc/${filename}-${outputRoot}.txt >> $outputLoc/$outputRoot.txt
done

rm tropicalSearch.exe

