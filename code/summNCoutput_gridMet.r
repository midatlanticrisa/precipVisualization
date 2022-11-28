##########################################################################
##########################################################################
## Script Name: summNCoutput_gridMet.r
## Purpose of Script: Summarizes clipped gridMet
## data (by county), specifically TMAX and Percip. The script is meant to be ran on ROAR.
##
## Special Requirements: A series of NetCDF files with gridMet time series data
## masked for each county
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 5/28/2021
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 7/29/2020
#
## Last Moddified By: Kelsey Ruckert
## Editors Email: klr324@psu.edu
## Date Last Edited: 9/16/2021
## Edits: Rscript accepts arguments for running with a Singularity container
## 10/5/2021: Script reads nc files with ncdf4 not raster and no longer uses mclapply
##            Only one script is required now.
##
## Copyright (c) 2021 The Pennsylvania State University
##
##########################################################################
##########################################################################

#!/usr/bin/env Rscript
# https://www.r-bloggers.com/2015/09/passing-arguments-to-an-r-script-from-command-lines/
args = commandArgs(trailingOnly=TRUE)

# test if there are two arguments: if not, return an error
if (length(args)<2) {
  stop("Two arguments must be supplied (directory name and state abbreviation).n", call.=FALSE)
} else {
  # load Arguments about states
  dataDirNm = args[1]       # "Maryland/"
  dataNm = args[2]          # "md"
}

print(paste("Using data from the directory:", dataDirNm))
print(paste("Output files will start with", dataNm, "in the name"))

library(raster)
library(foreach)
library(doParallel)
library(reshape2)
library(pbapply)
library(measurements)
#library(climdex.pcic)
library(ncdf4)

# Compile loops
library(compiler)
enableJIT(3)
enableJIT(3)

##load custom functions
source("ncSummFunctions_visual.R")

dataDir <- paste("/base/cntyTimeSeries_GridMet/", dataDirNm, sep="")
ncFiles <- list.files(dataDir, ".nc", full.names=T, recursive=F)
prFiles <- ncFiles[grep("pr.day", ncFiles)]
tMinFiles <- ncFiles[grep("tmin.day", ncFiles)]
tMaxFiles <- ncFiles[grep("tmax.day", ncFiles)]

centDecs <- c(1970, 2000, 2030)  ##gridMet
##########################################################################
# Precipitation climate statistics
##########################################################################
# 2 inch ------------------------------------------------------------------
calcWet2inDays <- lapply(prFiles, calcThresNumDays, decs=centDecs, 
                         thres=conv_unit(2.0,"inch","mm"), type="pr", 
                         fileNMSplt1=".1979", fileNMSplt2="grid.", inRCP=F, 
                         trueDate=F)
chkWetResults2in <- sapply(calcWet2inDays, length)
if(length(which(chkWetResults2in!=2))==0){
  gridvals2in <- (1:length(prFiles))
} else { gridvals2in <- (1:length(prFiles))[-which(chkWetResults2in!=2)] }

wetSummaries2in <- lapply(gridvals2in, function(chd){calcWet2inDays[[chd]][[1]]})
wetByYr2in <- lapply(gridvals2in, function(chd){calcWet2inDays[[chd]][[2]]})
oneTempTab2in <- do.call(rbind.data.frame, wetSummaries2in)
write.csv(oneTempTab2in, paste0(dataDir, dataNm, "2incountyGridMet_wetDays.csv"), row.names=F)
oneTempYrTab2in <- do.call(rbind.data.frame, wetByYr2in)
write.csv(oneTempYrTab2in, paste0(dataDir, dataNm, "2incountyGridMet_wetDaysByYr.csv"), row.names=F)

# 1 inch ------------------------------------------------------------------
calcWetDays <- lapply(prFiles, calcThresNumDays, decs=centDecs,
                      thres=conv_unit(1.0,"inch","mm"), type="pr",
                      fileNMSplt1=".1979", fileNMSplt2="grid.", inRCP=F,
                      trueDate=F)

chkWetResults1in <- sapply(calcWetDays, length)
if(length(which(chkWetResults1in!=2))==0){
  gridvals1in <- (1:length(prFiles))
} else { gridvals1in <- (1:length(prFiles))[-which(chkWetResults1in!=2)] }

wetDaysSummaries <- lapply(gridvals1in, function(chd){calcWetDays[[chd]][[1]]})
wetDaysByYr <- lapply(gridvals1in, function(chd){calcWetDays[[chd]][[2]]})

oneDaysTempTab <- do.call(rbind.data.frame, wetDaysSummaries)
write.csv(oneDaysTempTab, paste0(dataDir, dataNm, "1incountyGridMet_wetDays.csv"), row.names=F)
oneDaysTempYrTab <- do.call(rbind.data.frame, wetDaysByYr)
write.csv(oneDaysTempYrTab, paste0(dataDir, dataNm, "1incountyGridMet_wetDaysByYr.csv"), row.names=F)

# Total precip ------------------------------------------------------------
calcWetTot <- lapply(prFiles, calcThresWetTot, decs=centDecs, thres=NA, 
                     base =NA, type="tot", fileNMSplt1=".1979", 
                     fileNMSplt2="grid.", inRCP=F, trueDate=F)
chkWetResultsTot <- sapply(calcWetTot, length)
if(length(which(chkWetResultsTot!=2))==0){
  gridvalsTot <- (1:length(prFiles))
} else { gridvalsTot <- (1:length(prFiles))[-which(chkWetResultsTot!=2)] }

wetTotSummaries <- lapply(gridvalsTot, function(chd){calcWetTot[[chd]][[1]]})
wetTotByYr <- lapply(gridvalsTot, function(chd){calcWetTot[[chd]][[2]]})

oneTotTempTab <- do.call(rbind.data.frame, wetTotSummaries)
write.csv(oneTotTempTab, paste0(dataDir, dataNm, "countyGridMet_tot.csv"), row.names=F)
oneTotTempYrTab <- do.call(rbind.data.frame, wetTotByYr)
write.csv(oneTotTempYrTab, paste0(dataDir, dataNm, "countyGridMet_totByYr.csv"), row.names=F)

# Total precip 99th ------------------------------------------------------------
calcWetTot99th <- lapply(prFiles, calcThresWetTot, decs=centDecs, thres="99th", 
                     base =NA, type="tot_thres", fileNMSplt1=".1979", 
                     fileNMSplt2="grid.", inRCP=F, trueDate=F)
chkWetResultsTot99th <- sapply(calcWetTot99th, length)
if(length(which(chkWetResultsTot99th!=2))==0){
  gridvalsTot99th <- (1:length(prFiles))
} else { gridvalsTot99th <- (1:length(prFiles))[-which(chkWetResultsTot99th!=2)] }

wetTotSummaries99th <- lapply(gridvalsTot99th, function(chd){calcWetTot99th[[chd]][[1]]})
wetTotByYr99th <- lapply(gridvalsTot99th, function(chd){calcWetTot99th[[chd]][[2]]})

oneTotTempTab99th <- do.call(rbind.data.frame, wetTotSummaries99th)
write.csv(oneTotTempTab99th, paste0(dataDir, dataNm, "countyGridMet_99thtot.csv"), row.names=F)
oneTotTempYrTab99th <- do.call(rbind.data.frame, wetTotByYr99th)
write.csv(oneTotTempYrTab99th, paste0(dataDir, dataNm, "countyGridMet_99thtotByYr.csv"), row.names=F)

##########################################################################
##########################################################################
