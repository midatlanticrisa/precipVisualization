##########################################################################
##########################################################################
## Script Name: createLocationShapefiles.R
## Purpose of Script: A script to create a shapefile, of a location based
## on a coordinate and a specificied width/height. This was specifically 
## written to create masks to summarize the GridMet and MACA climate data
## for testing climate outlook visualizations.
##
## Special Requirements: a coordinate location (Atlas 14 station preferred) 
## and width/height in km.
##
## Author: Kelsey Ruckert
## Email: klr324@psu.edu
## Date Created: 11/23/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################

#!/usr/bin/env Rscript
# https://www.r-bloggers.com/2015/09/passing-arguments-to-an-r-script-from-command-lines/
args = commandArgs(trailingOnly=TRUE)

# test if there are two arguments: if not, return an error
if (length(args)<4) {
  stop("Four arguments must be supplied (directory name for input/output, path 
       to input directory, path to output directory, and boolean for MACA).n", call.=FALSE)
} else {
  # load Arguments about states
  shapefile = args[1]        # "Maryland/" "mtsunapeeNH.shp"
  pathDir = args[2]          # "/gpfs/group/kzk10/default/private/ren10/mlisk_collab/counties/shapefiles/"
  outDir = args[3]           # Should be the temp directory: "/gpfs/group/kzk10/default/private/outlooks-temp/shapefiles/"
  MACA = args[4]             # Boolean (true/false): if using maca dataset set to TRUE
  size = as.numeric(args[5]) # Size of the square to analyze. Ex: 100 will make a 100km x 100km square
}

library(rgdal)
library(raster)
library(measurements)

# Compile loops
library(compiler)
enableJIT(3)
enableJIT(3)

# Get the name without the shp
statenm = gsub(".shp", "", shapefile)

# # Get the state name without white space
# statenm = gsub("[[:space:]]", "", sub("/", "", dataDirNm))
# 
# # Set state fips. A list is available at:
# # https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
# statefips = read.csv(paste0(pathDir, "stateFIPS.csv"))
# 
# # extract the fips matching the state name
# if(statenm == "DistrictofColumbia"){
#   fips <- statefips$FIPS[match("District of Columbia", statefips$Name)] 
# } else if(statenm == "NewYork"){
#   fips <- statefips$FIPS[match("New York", statefips$Name)]
# } else if(statenm == "WestVirginia"){
#   fips <- statefips$FIPS[match("West Virginia", statefips$Name)]
# } else {
#   fips <- statefips$FIPS[match(statenm, statefips$Name)]
# }

print(paste("Creating shapefile for", statenm))

# Create output folder if folder doesn't already exist
outFolder = paste0(outDir, statenm,"_",size,"km")
ifelse(!dir.exists(outDir), dir.create(outDir), 
       paste("Nothing to do:", outDir, "already exists"))
ifelse(!dir.exists(outFolder), dir.create(outFolder), 
       paste("Nothing to do:", outFolder, "already exists"))

## Read and extract the US state
# "cb_2018_us_state_500k" is the 2018 spatial census data 
# (https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html), 
# which is used for general reference of US counties unless a higher detailed product is 
# required. It is also the layer in which was used to create the "drop" maps in the 
# header of the Outlooks.
usStates <- readOGR(dsn=paste0(pathDir, shapefile), #klr 7/18/2022: shapefile not within US subfolder 
                    layer=statenm)

if(statenm == "mtsunapeeNH"){
  # convert to NAD83(2011)/New Hampshire
  nad83 <- "EPSG:6524"
} else {
  # convert to NAD83(2011)/California Zone 5
  nad83 <- "EPSG:6423"
}

usStates <- spTransform(usStates, crs(nad83)) 

# set the radius for the plots
radius <- conv_unit(size/2, "km", "m")
#radius <- 50000 # radius in meters 50000 m  = 100x100km square plot.

# define the plot edges based upon the plot radius. 
yPlus <- usStates@coords[2] + radius
xPlus <- usStates@coords[1] + radius
yMinus <- usStates@coords[2] - radius
xMinus <- usStates@coords[1] - radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

# loop through each centroid value and create a polygon
# this is where we match the ID to the new plot coordinates
a<-Polygons(list(Polygon(matrix(square, ncol=2, byrow=TRUE))), usStates$Station.na) 
# convert a to SpatialPolygon and assign CRS
polysB<-SpatialPolygons(list(a),proj4string=CRS(nad83))

# convert the coordinate system to WGS84 for consistency with the GridMET netcdf
wgs84 <- "EPSG:4326"
# usStates <- spTransform(usStates, crs(wgs84)) 
bbox <- spTransform(polysB, crs(wgs84)) 

# # Extract just the state
# st <- usStates[which(usStates@data$STATEFP == fips), ]

# Loop through each of the counties in the state
# for(i in 1:length(st@data$NAME)){
  # cnty <- st[which(st@data$NAME == "Accomack"), ]
  
  # Extract the polygon, re-center or shift longitudes < 0 by adding 360 for MACA
  if(MACA){
    cnty <- as(recenter(bbox), "SpatialPolygonsDataFrame")
  } else {
    cnty <- as(bbox, "SpatialPolygonsDataFrame")
  }
  # Find bounding box and increase by 0.25 degree to ensure all areas are included
  # Grid resolution is 1/24 ~ 4km ~ 0.0416 degrees so 0.1667 should be good which is
  # 4 grids cells larger
  grids = (1/24)*4
  extent <- matrix(c(paste0('lon1="',cnty@bbox['x','min']-grids,'"'), 
                     paste0('lon2="',cnty@bbox['x','max']+grids,'"'), 
                     paste0('lat1="',cnty@bbox['y','min']-grids,'"'), 
                     paste0('lat2="',cnty@bbox['y','max']+grids,'"')), ncol=1)
  # Remove any periods ".", any apostrophes "'", and any spaces
  # cntyname <- gsub("\\.", "", cnty@polygons[[1]]@ID)
  # cntyname <- gsub("\\'", "", cntyname)
  # cntyname <- gsub("[[:space:]]", "", cntyname) # extract the county name
  # cntyFP <- st@data$GEOID[i] # extract the county fips in case duplicates exist
  
  # Save the extent file
  write.table(extent, paste0(outFolder, "/cnty.", statenm,"_",size,"km", 
                             ".extent.txt"), row.names = FALSE,
              col.names=FALSE, quote=FALSE)
  # Save the counties as individual files
  writeOGR(cnty, outFolder, paste0("cnty.", statenm, "_",size,"km"), 
           driver = "ESRI Shapefile")
# }
