#!/bin/bash

atmDataDir=/Volumes/Storage/polarStorms/ne240
ocnDataDir=/Volumes/Storage/polarStorms/SSTData

outputLoc=$HOME/Desktop/ssearchResults/polarLows

exeLoc=$HOME/Desktop/StrideSearch/install/bin

outputRoot=ne240-pLow-p980-z2em4-t7

rm -f $outputLoc/$outputRoot-outputFile.txt

cd $atmDataDir
cp $exeLoc/polarSearchDriver.exe .

for filename in *.nc
do 
cat <<EOF > polarLow.namelist
&input
	atmfilename = '${atmDataDir}/${filename}'
	ocnfilename = '${ocnDataDir}/sst_HadOIBl_bc_1x1_clim_pi_c101029-513x1024.nc' 
	southernBoundary = 50.0
	northernBoundary = 90.0
	sectorRadius = 450.0
	pslThreshold = 98000.0
	windThreshold = 0.0
	vortThreshold = 2.0e-04
	airSeaTempThreshold = 7.0
	vortDistThreshold = 500.0
	iceThreshold = 1.0
	outputDir = '${outputLoc}'
	outputRoot = '${filename}-${outputRoot}'	
/
EOF

./polarSearchDriver.exe polarLow.namelist

cat $outputLoc/${filename}-${outputRoot}.txt >> $outputLoc/$outputRoot-outputFile.txt

done




