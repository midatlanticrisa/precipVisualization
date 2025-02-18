# precipVisualization

This directory contains the code used to generate the analysis and precipitation information found in [] and the survey. Analysis codes are predomintely written in R.

Full Citation:

> Ruckert, K. L., Srikrishnan, V., and Keller, K. (in review). Characterizing the deep uncertainties surrounding coastal flood hazard projections: A case study for Norfolk, VA. *Scientific Reports*, ...



This repository contains shapefiles of Selinsgrove and documentation to  reproduce mapping the changes in FEMA flood hazard zones overtime in Selinsgrove. 

## Overview
In this analysis, we examine the differences between the extent of the 100-year floodplain established by FEMA in 1982, 2007, and 2021 in the Borough of Selinsgrove, PA. This analysis compares the effective flood hazard zones by 1) digitizing the 1982 map via georeferencing, 2) extracting the areas removed or added overtime, and 3) generating a map  to show how the extent has changed overtime. 

A few other shapefiles relevant to Selinsgrove are provided here for archival purposes (e.g., houses in Selinsgrove).

## Running the analysis
This repository includes everything you need to reproduce the analysis. However, you will more than likely need to update or modify paths to folders/files. 

To run the analysis, you will need ArcGIS-ArcMAP, QGIS, or some other GIS software. The documentation (`FloodChangesMap_tutorial.pdf`) and python script (`FEMA_FHZ_changes.py`) are specific to ArcMAP, but the steps in the documentation can be used as a guide for other software.