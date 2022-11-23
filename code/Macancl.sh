##########################################################################
##########################################################################
## Script Name: maskTimeSeries_01.pbs
## Purpose of Script: One of a series of scripts which clips MACA climate data
## by CONUS county masks, and saves them to individual .nc files. Utilizes 
## extensive use of CDO functions. The script series was originally
## incorporated into one file, but when it became appearent one file
## would take too long to process copies were made to speed up processing.
## There are a total of 20, one per GCM. As they are copies, only the first has
## been placed on GitHub. Meant to be ran on ROAR.
##
## Special Requirements: CDO; MACA climate data; masked rasters (as .nc) for 
## each CONUS county
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 5/28/2021
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 7/29/2020
##
## Copyright (c) 2021 The Pennsylvania State University
##
##########################################################################
##########################################################################

#!/bin/bash

## Input arguments:
## baseDir $1
## dataDir $2
## tempDir $3
## models $4

baseDir=$1
dataDir=$2
tempDir=$3
state=$4
#models
models=$5
shapePath=$6

# Add some metadata
who="Kelsey Ruckert"
time=$(date)
metadata="Masked by "$who" on "$time"."

CntyDir=$shapePath"/"$state"/"
outDir=$baseDir"/TimeSeries_MACA/"$state"/"

# Extract the county names based on the county shapefiles for the state
countyNames=$(find $CntyDir -not -path '*/.*' -type f -name '*.shp')
# Only get Menominee Nation
#countyNames=$(find $allcountyNames -not -path '*/.*' -type f -name '*Menominee*')

nc45Files=$(find $dataDir"/historical+rcp45/" -not -path '*/.*' -type f -name '*.nc')
pr45Files=$(find $nc45Files -name "*pr.day*")
tmin45Files=$(find $nc45Files -name "*tasmin.day*")
tmax45Files=$(find $nc45Files -name "*tasmax.day*")
nc85Files=$(find $dataDir"/historical+rcp85/" -not -path '*/.*' -type f -name '*.nc')
pr85Files=$(find $nc85Files -name "*pr.day*")
tmin85Files=$(find $nc85Files -name "*tasmin.day*")
tmax85Files=$(find $nc85Files -name "*tasmax.day*")

IFS=';' read -ra modelArray <<< "$models"

# Loop through each model
for mod in "${modelArray[@]}"; 
do
  # Grab each original model dataset
  modPRFile45=$(find $pr45Files -name "*$mod*")
  modPRFile85=$(find $pr85Files -name "*$mod*")
  modTMinFile45=$(find $tmin45Files -name "*$mod*")
  modTMaxFile45=$(find $tmax45Files -name "*$mod*")
  modTMinFile85=$(find $tmin85Files -name "*$mod*")
  modTMaxFile85=$(find $tmax85Files -name "*$mod*")
  
  # Create the names for the subsetted MACA daata
  outPRArea45=$tempDir"/"$state"/pr.day.r1i1p1.historical+rcp45."$mod".0.orig_grid.sub.19500101-20991231.nc"
  outPRArea85=$tempDir"/"$state"/pr.day.r1i1p1.historical+rcp85."$mod".0.orig_grid.sub.19500101-20991231.nc"
  outTminArea45=$tempDir"/"$state"/tasmin.day.r1i1p1.historical+rcp45."$mod".0.orig_grid.sub.19500101-20991231.nc"
  outTmaxArea45=$tempDir"/"$state"/tasmax.day.r1i1p1.historical+rcp45."$mod".0.orig_grid.sub.19500101-20991231.nc"
  outTminArea85=$tempDir"/"$state"/tasmin.day.r1i1p1.historical+rcp85."$mod".0.orig_grid.sub.19500101-20991231.nc"
  outTmaxArea85=$tempDir"/"$state"/tasmax.day.r1i1p1.historical+rcp85."$mod".0.orig_grid.sub.19500101-20991231.nc"

  ##first round masking by whole area - WV
  ##original masking by whole county area
  ## Select only the data covering West Virginia
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modPRFile45 $outPRArea45
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modPRFile85 $outPRArea85
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modTMinFile45 $outTminArea45
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modTMaxFile45 $outTmaxArea45
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modTMinFile85 $outTminArea85
  #cdo sellonlatbox,-93.647461,-86.550293,42.342305,47.115000 $modTMaxFile85 $outTmaxArea85
 
  for cnty in $countyNames;
  do
    IFS='.' read -ra splitName <<< "$cnty"

    # Set environmental variables with file names 
    nameMask=${splitName[1]}
    tempMask=$tempDir"/"$state"/"$nameMask"_mask.nc"
    #tempMask=$tempDir"/"$state"/"$mod"_"$nameMask"_mask.nc"
    #modelname=$mod"_"
    
    cntyExt=$CntyDir"cnty."$nameMask".extent.txt"

    # Clip to just the county level to reduce file size and improve R loading speed later on.
    # Use the extent + 4 times the grid resolution from the shapefile as the bounding box
    source $cntyExt
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modPRFile45 $outPRArea45
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modPRFile85 $outPRArea85
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modTMinFile45 $outTminArea45
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modTMaxFile45 $outTmaxArea45
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modTMinFile85 $outTminArea85
    cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $modTMaxFile85 $outTmaxArea85

    #NACntyPRMask45=$tempDir"/"$state"/pr.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    #NACntyPRMask85=$tempDir"/"$state"/pr.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    #NACntyTminMask45=$tempDir"/"$state"/tasmin.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    #NACntyTminMask85=$tempDir"/"$state"/tasmin.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    #NACntyTmaxMask45=$tempDir"/"$state"/tasmax.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    #NACntyTmaxMask85=$tempDir"/"$state"/tasmax.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
     
    outCntyPRMask45=$outDir"pr.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    outCntyPRMask85=$outDir"pr.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    outCntyTminMask45=$outDir"tasmin.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    outCntyTminMask85=$outDir"tasmin.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    outCntyTmaxMask45=$outDir"tasmax.day.r1i1p1.historical+rcp45."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
    outCntyTmaxMask85=$outDir"tasmax.day.r1i1p1.historical+rcp85."$mod".0.orig_grid."${splitName[1]}"_"${splitName[2]}".19500101-20991231.nc"
  
  # Create the mask using the boundary of a shapefile and the subsetted area
  # This process was created following the example at:
  # https://code.mpimet.mpg.de/boards/2/topics/6693
  # The mask can be used for tmin, tmax, and pr
  if [ -e "$tempMask" ]
  then
    echo $tempMask "exists nothing to be done"
  else
    # Create the county mask
    blankmodel="" 
    ncl tempPath=\"$tempDir\" shapePath=\"$shapePath\" state=\"$state\" model=\"$blankmodel\" cntyfipsname=\"$nameMask\" tmaxGrid=\"$outTmaxArea45\" maskGrid.ncl
  fi

  #ncl tempPath=\"$tempDir\" shapePath=\"$shapePath\" state=\"$state\" model=\"$modelname\" cntyfipsname=\"$nameMask\" tmaxGrid=\"$outTmaxArea45\" maskGrid.ncl
  
  # Mask the grid by doing division with the temp mask, the mask is based on 0s and 1s
  # Max temperature
  cdo div $outTmaxArea45 $tempMask $outCntyTmaxMask45
  cdo div $outTmaxArea85 $tempMask $outCntyTmaxMask85
  # Min temperature
  cdo div $outTminArea45 $tempMask $outCntyTminMask45
  cdo div $outTminArea85 $tempMask $outCntyTminMask85
  # Precipitation
  cdo div $outPRArea45 $tempMask $outCntyPRMask45
  cdo div $outPRArea85 $tempMask $outCntyPRMask85
  
  # Clip to just the county level to reduce file size and improve R loading speed later on.
  # Use the extent + 4 times the grid resolution from the shapefile as the bounding box
  #source $cntyExt
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyTmaxMask45 $outCntyTmaxMask45
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyTmaxMask85 $outCntyTmaxMask85
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyTminMask45 $outCntyTminMask45
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyTminMask85 $outCntyTminMask85
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyPRMask45 $outCntyPRMask45
  #cdo sellonlatbox,$lon1,$lon2,$lat1,$lat2 $NACntyPRMask85 $outCntyPRMask85

  # Add a global attribute stating who made the netcdf files and when
  # https://linux.die.net/man/1/ncatted
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyTmaxMask45
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyTmaxMask85
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyTminMask45
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyTminMask85
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyPRMask45
  ncatted -O -a "masking",global,c,c,"$metadata" $outCntyPRMask85

  done
done



