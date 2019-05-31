#!/bin/bash

dataDir=$HOME/Desktop/ssearchResults/polarLows

exeLoc=$HOME/Desktop/StrideSearch/install/bin

outputLoc=$dataDir

cd $dataDir
cp $exeLoc/trackDriver.exe .

cat <<EOF > trackDriver.namelist
&input
detectionOutputFile='${outputLoc}/ne240-pLow-p980-z2em4-t7-outputFile.txt'
minDuration = 2
hoursPerTimestep = 6
maxTravelSpeed = 20.0
outputDir='${outputLoc}'
outputRoot='polarTrackDriver-pLow-p980-z2em4-t7'
/
EOF

./trackDriver.exe trackDriver.namelist

