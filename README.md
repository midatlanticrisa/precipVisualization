# Code for the extreme precipitation survey and analyses

Our study consisted of four phases: graph development, survey design, data collection, and data analysis. This repository contains the code used for graph development and design for in the survey, the survey and the collected responses from U.S. adults, and the code analyzing the responses and generating the figures in the two journal articles. Analysis codes are predomintely written in R.

Full Citation:

> Ruckert, K.L., Cooper, C., Roth, S., and Nicholas, R.E. (in review). Survey of U.S. adults reveals simpler graph types improve interpretation of extreme precipitation. *Weather, Climate, and Society*, ...

> Ruckert, K.L., Cooper, C., and Nicholas, R.E. (in prep). Survey of U.S. Adults Reveals Visual Interpretation, Climate Literacy, Demographics, and Personal Values Drive Flood Preparedness Decisions. *In prep.*, ...

## Survey graph development and design
The survey used three common graph types for extreme precipitation following the style used in the [Local Hazard Outlooks](https://www.marisa.psu.edu/outlooks/). To generate the extreme precipitation data, we use the code and data available in the [Outlooks Repo](https://github.com/midatlanticrisa/communityClimateOutlooks). Since, the Outlooks cover the Mid-Atlantic region on a county scale, we provide the scripts for the 100x100 km area center on Mt. Sunapee, NH in this repo. 

### Create location shapefile mask
To begin the extraction process, first create the shapefile to mask 100kmx100km box around Mt. Sunapee using `code/`:

- createLocationShapefiles.R and
- createShapefilesMtSunapee.pbs

The location of the Atlas 14 station providing the centroid is the `shapefiles/mtsunapeeNH.shp` file.

### Clip the climate data to the location
With the mask created, commands from [Climate Data Operators (CDO)](https://code.mpimet.mpg.de/projects/cdo) and [NCAR Command Language](https://www.ncl.ucar.edu) were used to apply the Mt. Sunapee mask to the climate data. This was done with `code/`:

 * `maskGrid.ncl` and `shapefile_utils.ncl` which are functions that create a netcdf mask given a gridded netcdf file and a boundary shapefile.
 * `generateGridMetSunapee100km.pbs` launches `GridMetncl.sh` to subset the pr .nc files for GridMet from CONUS extent to the location extent.
 * `generateMACASunapee100km.pbs` launches `Macancl.sh` which subsets the pr .nc files for MACA from CONUS extent to the location extent.

### Summarize climate indices
The directory `code/` also contains the scripts to summarize the time series. `summNCoutput_gridMet.R` and `summNCoutput_MACA.R` are used to summarize the time series for their respective products. They are launched by pbs scripts `summNCgridMtSunapee100km.pbs` and `summNCmacaMtSunapee100km.pbs` and call `ncSummFunctions_visual.R`. `ncSummFunctions_visual.R` is a series of functions that read the netcdf files and summarize the climate indices. `organizemacaMtSunapee.pbs` and `organizeMACA.R` combine the MACA summary files into one large file. The output should be a series of .csv's, for various precipitation indices such as days ≥ 0.5 mm, days ≥ 99th threshold, and 99th threshold precipitation. The resulting data is saved in the `TimeSeries_GridMet` and `TimeSeries_Maca`.

### Create graphs and visuals for the survey
This repository contains code that creates the figures in the survey with `precip_vis_experimentDesign.R`. This script calls `precip_functions.R` which contains the functions for creating plots. The plots are saved in `plots/mtsunapeeNH_100km`.

## Obtaining the data
As stated above, all of the data needed to run the analysis and create the plots are provided in the `Data/` directory. However, we also include the scripts used to extract the data or state how we extracted the data for reproducibility. If you do not wish to rerun the data collection scripts, you can skip this part and just run the analysis and plot the figures.

## Survey analysis 

In this analyses, we conducted an online survey of U.S. adults. This survey tested 1) how graph type influenced the interpretation, usability, and decision-making related to information about extreme precipitation in flood risk scenarios, and 2) how demographics, climate science knowledge, and interpretation influence decisions. 

## Running the analysis
The directory `analysis/` includes everything needed to reproduce the work described in Ruckert et al. (in prep.). The data files that correspond to the paper are available in the `data/` directory. This includes the survey responses downloaded from Qualtrics `data/HeavyPrecip_June+5,+2024_11.24.csv` and the list of responses to remove due to quality issues `data/responses2remove_5June2024.csv`

Once you have downloaded the repository, you can open R, install the relevant R packages, and run the analysis. Note that if these packages are already installed and/ or loaded in R, R will throw error messages with a request to restart R before proceeding with package updates.

```R 
install.packages(report)
install.packages(ggplot2)
install.packages(car)
install.packages(multcomp)
install.packages(FSA)
install.packages(stringr)
install.packages(dplyr)
install.packages(RColorBrewer)
install.packages(vioplot)
install.packages(coin)
install.packages(rcompanion)
install.packages(TeachingDemos)
install.packages(cowplot)
install.packages(gridGraphics)
install.packages(scales)
install.packages(tidytext)
install.packages(gt)
install.packages(webshot2)
install.packages(tidyverse)
install.packages(wordcloud)
install.packages(ggpubr)
install.packages(magick)

setwd("PATH_TO_DIRECTORY/precipVisualization/analysis") 
```

The scripts `analysis/paper1_Vis.R` and `analysis/paper2_Dec.R` analyzes the impact of graph type on interpretation, demographic and other factors on decision-making, and creates the publication figures. The file `scripts/readSurvey.R` reads in the survey responses and formats the data by survey group while `scripts/surveyFunctions.R` contains functions for parsing and evaluation.

## Paper 1
> Ruckert, K.L., Cooper, C., Roth, S., and Nicholas, R.E. (in review). Survey of U.S. adults reveals simpler graph types improve interpretation of extreme precipitation. *Weather, Climate, and Society*, ...



```R 
source("paper1_Vis.R")
```

## Paper 2
> Ruckert, K.L., Cooper, C., and Nicholas, R.E. (in prep). Survey of U.S. Adults Reveals Visual Interpretation, Climate Literacy, Demographics, and Personal Values Drive Flood Preparedness Decisions. *In prep.*, ...

The script  calls the script that analyzes the impact of graph type and creates the figures. The file `scripts/readSurvey.R` reads in the survey responses and formats the data by survey group while `scripts/surveyFunctions.R` contains functions for parsing and evaluation.

```R 
source("paper2_Dec.R")
```

## Overview
In this analysis, we examine the differences between the extent of the 100-year floodplain established by FEMA in 1982, 2007, and 2021 in the Borough of Selinsgrove, PA. This analysis compares the effective flood hazard zones by 1) digitizing the 1982 map via georeferencing, 2) extracting the areas removed or added overtime, and 3) generating a map  to show how the extent has changed overtime. 

A few other shapefiles relevant to Selinsgrove are provided here for archival purposes (e.g., houses in Selinsgrove).

## Running the analysis
This repository includes everything you need to reproduce the analysis. However, you will more than likely need to update or modify paths to folders/files. 

To run the analysis, you will need ArcGIS-ArcMAP, QGIS, or some other GIS software. The documentation (`FloodChangesMap_tutorial.pdf`) and python script (`FEMA_FHZ_changes.py`) are specific to ArcMAP, but the steps in the documentation can be used as a guide for other software.

## Contacts
Kelsey Ruckert (klr324@psu.edu)  
Robert Nicholas (ren10@psu.edu) 
Courtney Cooper (ccooper@berry.edu)  
Samantha Roth (Samantha.M.Roth@dartmouth.edu)

## License
Copyright 2025 Kelsey Ruckert

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

These files are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with these files. If not, see http://www.gnu.org/licenses/.