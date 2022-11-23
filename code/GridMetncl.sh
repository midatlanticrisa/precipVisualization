# A script to mask the GridMet data to Wisconsin, Menominee county
#!/bin/bash

## Input arguments:
baseDir=$1
dataDir=$2
tempDir=$3
state=$4
model="" 
shapePath=$5 

# Add some metadata
who="Kelsey Ruckert"
time=$(date)
metadata="Masked by "$who" on "$time"."

# Set input and output paths
CntyDir=$shapePath"/"$state"/"
outDir=$baseDir"/TimeSeries_GridMet/"$state"/"

# Extract the county names based on the county shapefiles for the state
countyNames=$(find $CntyDir -not -path '*/.*' -type f -name '*.shp')
# Only get Menominee Nation
#countyNames=$(find $allcountyNames -not -path '*/.*' -type f -name '*Menominee*')

# Get names of the orginial gridMet data files
prFile=$dataDir"/pr.day.orig_grid.19790101-20201231.nc"
tmaxFile=$dataDir"/tmmx.day.orig_grid.19790101-20201231.nc"
tminFile=$dataDir"/tmmn.day.orig_grid.19790101-20201231.nc"

# Create the names for the subsetted gridMet daata
outPRArea=$tempDir"/"$state"/pr.day.sub.19790101-20201231.nc"
outTMaxArea=$tempDir"/"$state"/tmax.day.sub.19790101-20201231.nc"
outTMinArea=$tempDir"/"$state"/tmin.day.sub.19790101-20201231.nc"

##original masking by whole county area
## Select only the data covering Delaware
## sellonlatbox,lon1,lon2,lat1,lat2 infile outfile
##-93.647461,42.342305,-86.550293,47.115000
## http://bboxfinder.com
#cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $prFile $outPRArea
#cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $tmaxFile $outTMaxArea
#cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $tminFile $outTMinArea

# Loop through each county to generate a gridded mask and then create a county netcdf using the mask
for cnty in $countyNames;
do
  IFS='.' read -ra splitName <<< "$cnty"
  
  # Set environmental variables with file names 
  nameMask=${splitName[1]}"."${splitName[2]}
  tempMask=$tempDir"/"$state"/"$nameMask"_mask.nc"
  cntyExt=$CntyDir"cnty."$nameMask"extent.txt"

  # # Clip to just the county level to reduce file size and improve R loading speed later on.
  # # Use the extent + 4 times the grid resolution from the shapefile as the bounding box
  # cdo ifthen $tempMask $NATMaxCntyMask $outTMaxCntyMask
  # cdo ifthen $tempMask $NAPRCntyMask $outPRCntyMask
  source $cntyExt
  cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $prFile $outPRArea
  cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $tmaxFile $outTMaxArea
  cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $tminFile $outTMinArea

  #NAPRCntyMask=$tempDir"/"$state"/pr.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"
  #NATMaxCntyMask=$tempDir"/"$state"/tmax.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"
  #NATMinCntyMask=$tempDir"/"$state"/tmin.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"
  outPRCntyMask=$outDir"pr.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"
  outTMaxCntyMask=$outDir"tmax.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"
  outTMinCntyMask=$outDir"tmin.day.orig_grid."${splitName[1]}"_"${splitName[2]}".19790101-20201231.nc"  

  # Create the mask using the boundary of a shapefile and the subsetted area
  # This process was created following the example at:
  # https://code.mpimet.mpg.de/boards/2/topics/6693
  # The mask can be used for tmin, tmax, and pr
  ncl tempPath=\"$tempDir\" shapePath=\"$shapePath\" state=\"$state\" model=\"$model\" cntyfipsname=\"$nameMask\" tmaxGrid=\"$outTMaxArea\" maskGrid.ncl

  # Mask the grid by doing division with the temp mask, the mask is based on 0s and 1s
  # Max temperature
  cdo div $outTMaxArea $tempMask $outTMaxCntyMask
  # Min temperature
  cdo div $outTMinArea $tempMask $outTMinCntyMask
  # Precipitation
  cdo div $outPRArea $tempMask $outPRCntyMask

 # # Clip to just the county level to reduce file size and improve R loading speed later on.
 # # Use the extent + 4 times the grid resolution from the shapefile as the bounding box
 # cdo ifthen $tempMask $NATMaxCntyMask $outTMaxCntyMask
 # cdo ifthen $tempMask $NAPRCntyMask $outPRCntyMask
 # source $cntyExt
 # cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NATMaxCntyMask $outTMaxCntyMask
 # cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NATMinCntyMask $outTMinCntyMask
 # cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NAPRCntyMask $outPRCntyMask

  # Add a global attribute stating who made the netcdf files and when
  # https://linux.die.net/man/1/ncatted
  ncatted -O -a "masking",global,c,c,"$metadata" $outTMaxCntyMask
  ncatted -O -a "masking",global,c,c,"$metadata" $outTMinCntyMask
  ncatted -O -a "masking",global,c,c,"$metadata" $outPRCntyMask

done
