# Code for the extreme precipitation survey and analyses

This directory contains the code used to generate the figures and precipitation information found in the survey of US adults along with the code analyzing the data. Analysis codes are predomintely written in R.

Full Citation:

> Ruckert, K.L., Cooper, C., Roth, S., and Nicholas, R.E. (in review). Survey of U.S. adults reveals simpler graph types improve interpretation of extreme precipitation. *Weather, Climate, and Society*, ...

> Ruckert, K.L., Cooper, C., and Nicholas, R.E. (in prep). Survey of U.S. Adults Reveals Visual Interpretation, Climate Literacy, Demographics, and Personal Values Drive Flood Preparedness Decisions. *Weather, Climate, and Society*, ...

## Survey data

See more info at [https://github.com/midatlanticrisa/communityClimateOutlooks](https://github.com/midatlanticrisa/communityClimateOutlooks)

### Create location shapefile mask
To begin the extraction process, a series of US county masks as shapefiles were created for each product, using `code/`:

- createLocationShapefiles.R and
- createShapefilesMtSunapee.pbs

The county boundaries are the `counties/shapefiles/cb_2018_us_county_500k` ([500K US Census boundaries](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)) and the list of state FIPS codes are in `counties/shapefiles/stateFIPS.csv`.

### Clip the data to the location
With the masks created, commands from [Climate Data Operators (CDO)](https://code.mpimet.mpg.de/projects/cdo) and [NCAR Command Language](https://www.ncl.ucar.edu) were used to apply the Mt. Sunapee mask to the climate data. This was done with `code/`:

 * `maskGrid.ncl` and `shapefile_utils.ncl` which are functions that create a netcdf mask given a gridded netcdf file and a boundary shapefile.
 * `generateGridMetSunapee100km.pbs` launches `GridMetncl.sh` to subset the tmax, tmin, and pr .nc files for GridMet from CONUS extent to the location extent.
 * `generateMACASunapee100km.pbs` launches `Macancl.sh` which subsets the tmax, tmin, and pr .nc files for MACA from CONUS extent to the location extent.

### Summarize climate indices and create plots
The directory `code/` also contains the scripts to summarize the time series. `summNCoutput_gridMet.R` and `summNCoutput_MACA.R` are used to summarize the time series for their respective products. They are launched by pbs scripts `summNCgridMtSunapee100km.pbs` and `summNCmacaMtSunapee100km.pbs` and call `ncSummFunctions_visual.R`. `ncSummFunctions_visual.R` is a series of functions that read the netcdf files and summarize the climate indices. `organizemacaMtSunapee.pbs` and `organizeMACA.R` combine the MACA summary files into one large file. The output should be a series of `.csv`s, for various climate indices such as days ≥ 0.5 mm, days ≥ 99th threshold, and 99th threshold precipitation. The resulting data is saved in the `TimeSeries_GridMet` and `TimeSeries_Maca`

This repository contains code that creates the figures in the survey with `precip_vis_experimentDesign.R`. This script calls `precip_functions.R` which contains the functions for creating plots. The plots are saved in `plots/mtsunapeeNH_100km`.

## Overview
In this analysis, we examine the differences between the extent of the 100-year floodplain established by FEMA in 1982, 2007, and 2021 in the Borough of Selinsgrove, PA. This analysis compares the effective flood hazard zones by 1) digitizing the 1982 map via georeferencing, 2) extracting the areas removed or added overtime, and 3) generating a map  to show how the extent has changed overtime. 

A few other shapefiles relevant to Selinsgrove are provided here for archival purposes (e.g., houses in Selinsgrove).

## Running the analysis
This repository includes everything you need to reproduce the analysis. However, you will more than likely need to update or modify paths to folders/files. 

To run the analysis, you will need ArcGIS-ArcMAP, QGIS, or some other GIS software. The documentation (`FloodChangesMap_tutorial.pdf`) and python script (`FEMA_FHZ_changes.py`) are specific to ArcMAP, but the steps in the documentation can be used as a guide for other software.