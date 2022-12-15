##########################################################################
##########################################################################
## Script Name: summNCoutput_MACA.r
## Purpose of Script: One of a series of scripts which summarizes clipped MACA
## data (by county), specifically TMAX and Percip. The script series was 
## originally incorporated into one file, but when it became appearent one file
## would take too long to process copies were made to speed up processing.
## There are a total of 20, one per GCM. As they are copies, only the first has
## been placed on GitHub. These scripts are meant to be ran on ROAR.
##
## Special Requirements: A series of NetCDF files with MACA time series data
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
  options = " BNU-ESM\n CCSM4\n CNRM-CM5\n GFDL-ESM2G\n GFDL-ESM2M\n HadGEM2-CC365\n HadGEM2-ES365\n IPSL-CM5A-LR\n IPSL-CM5A-MR\n IPSL-CM5B-LR\n MIROC-ESM.0\n MIROC-ESM-CHEM\n MIROC5\n MRI-CGCM3\n NorESM1-M\n bcc-csm1-1.0\n bcc-csm1-1-m\n inmcm4\n CanESM2\n CSIRO-Mk3-6"
  stop(paste0("Three arguments must be supplied (directory name, state abbreviation, and model name).\nModel options include:\n", options), call.=FALSE)
} else {
  # load Arguments about states
  dataDirNm = args[1]       # "Maryland/"
  dataNm = args[2]          # "md"
  model = args[3]           # "BNU-ESM"
}

# dataDirNm = "Virginia/"       # "Maryland/"
# dataNm = "va"          # "md"
# model = "BNU-ESM"           # "BNU-ESM"

print(paste("Using data from the directory:", dataDirNm))
print(paste("Output files will start with", dataNm, "in the name"))
print(paste("Analyzing model", model))

library(raster)
library(foreach)
library(doParallel)
library(reshape2)
library(pbapply)
library(measurements)
#library(climdex.pcic)
library(ncdf4)
# install.packages("lmom")
# library(lmom)

# Compile loops
library(compiler)
enableJIT(3)
enableJIT(3)

##load custom functions
source("ncSummFunctions_visual.R")
# 
# if(model == "CCSM4"){
#   
# } else if(model == "MIROC5"){
#   
# } else {
#   dataDir <- paste("/base/kzk10/default/private/ren10/mlisk_collab/cntyTimeSeries_MACA/", dataDirNm, sep="")
# }

dataDir <- paste("/base/TimeSeries_MACA/", dataDirNm, sep="")
outDir <- paste("/base/TimeSeries_MACA/", dataDirNm, sep="")
ncFiles <- list.files(dataDir, ".nc", full.names=T, recursive=F)
prFiles <- ncFiles[grep("pr.day", ncFiles)]

prFiles <- prFiles[grep(model, prFiles)]

#prFiles <- "~/Documents/pr.day.r1i1p1.historical+rcp85.MRI-CGCM3.0.orig_grid.Chesapeake_Virginia_51550.19500101-20991231.nc"

centDecs <- c(1950, 2020)
futDecs <- c(1950, 2050)
climDecs <- c(1970, 2000, 2030, 2060)

#nc <- prFiles
# ##########################################################################
# # Precipitation climate statistics
# ##########################################################################
# # 2 inch ------------------------------------------------------------------
# calcWet2inDays <- lapply(prFiles, calcThresNumDays, decs=climDecs,
#                          thres=conv_unit(2.0,"inch","mm"), type="pr",
#                          fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                          trueDate=T)
# chkWetResults2in <- sapply(calcWet2inDays, length)
# if(length(which(chkWetResults2in!=2))==0){
#   gridvals2in <- (1:length(prFiles))
# } else { gridvals2in <- (1:length(prFiles))[-which(chkWetResults2in!=2)] }
# 
# wetSummaries2in <- lapply(gridvals2in, function(chd){calcWet2inDays[[chd]][[1]]})
# wetByYr2in <- lapply(gridvals2in, function(chd){calcWet2inDays[[chd]][[2]]})
# oneTempTab2in <- do.call(rbind.data.frame, wetSummaries2in)
# write.csv(oneTempTab2in, paste0(dataDir, dataNm, "countyMACA_2inwetDays_", model, ".csv"), row.names=F)
# oneTempYrTab2in <- do.call(rbind.data.frame, wetByYr2in)
# write.csv(oneTempYrTab2in, paste0(dataDir, dataNm, "countyMACA_2inwetDaysByYr_", model, ".csv"), row.names=F)
# 
# # 1 inch ------------------------------------------------------------------
# calcWetDays <- lapply(prFiles, calcThresNumDays, decs=climDecs,
#                       thres=conv_unit(1.0,"inch","mm"), type="pr",
#                       fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                       trueDate=T)
# chkDaysWetResults <- sapply(calcWetDays, length)
# if(length(which(chkDaysWetResults!=2))==0){
#   gridvals1in <- (1:length(prFiles))
# } else { gridvals1in <- (1:length(prFiles))[-which(chkDaysWetResults!=2)] }
# 
# wetDaysSummaries <- lapply(gridvals1in, function(chd){calcWetDays[[chd]][[1]]})
# wetDaysByYr <- lapply(gridvals1in, function(chd){calcWetDays[[chd]][[2]]})
# oneDaysTempTab <- do.call(rbind.data.frame, wetDaysSummaries)
# write.csv(oneDaysTempTab, paste0(dataDir, dataNm, "countyMACA_1inwetDays_", model, ".csv"), row.names=F)
# oneDaysTempYrTab <- do.call(rbind.data.frame, wetDaysByYr)
# write.csv(oneDaysTempYrTab, paste0(dataDir, dataNm, "countyMACA_1inwetDaysByYr_", model, ".csv"), row.names=F)
# 
# # Total precip ------------------------------------------------------------
# calcWetTot <- lapply(prFiles, calcThresWetTot, decs=climDecs, thres=NA,
#                      base =NA, type="tot", fileNMSplt1=".1950",
#                      fileNMSplt2="grid.", inRCP=T, trueDate=T)
# chkTotWetResults <- sapply(calcWetTot, length)
# if(length(which(chkTotWetResults!=2))==0){
#   gridvalsTot <- (1:length(prFiles))
# } else { gridvalsTot <- (1:length(prFiles))[-which(chkTotWetResults!=2)] }
# 
# wetTotSummaries <- lapply(gridvalsTot, function(chd){calcWetTot[[chd]][[1]]})
# wetTotByYr <- lapply(gridvalsTot, function(chd){calcWetTot[[chd]][[2]]})
# oneTotTempTab <- do.call(rbind.data.frame, wetTotSummaries)
# write.csv(oneTotTempTab, paste0(dataDir, dataNm, "countyMACA_tot_", model, ".csv"), row.names=F)
# oneTotTempYrTab <- do.call(rbind.data.frame, wetTotByYr)
# write.csv(oneTotTempYrTab, paste0(dataDir, dataNm, "countyMACA_totByYr_", model, ".csv"), row.names=F)

# 0.5 mm (Days with rain) ------------------------------------------------------------------
calcWetDays0p5mm <- lapply(prFiles, calcThresNumDays, decs=climDecs,
                      thres=0.5, type="pr",
                      fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
                      trueDate=T)
chkDaysWetResults0p5mm <- sapply(calcWetDays0p5mm, length)
if(length(which(chkDaysWetResults0p5mm!=2))==0){
  gridvals0p5mm <- (1:length(prFiles))
} else { gridvals0p5mm <- (1:length(prFiles))[-which(chkDaysWetResults0p5mm!=2)] }

wetDaysSummaries0p5mm <- lapply(gridvals0p5mm, function(chd){calcWetDays0p5mm[[chd]][[1]]})
wetDaysByYr0p5mm <- lapply(gridvals0p5mm, function(chd){calcWetDays0p5mm[[chd]][[2]]})
oneDaysTempTab0p5mm <- do.call(rbind.data.frame, wetDaysSummaries0p5mm)
write.csv(oneDaysTempTab0p5mm, paste0(dataDir, dataNm, "countyMACA_0p5mmwetDays_", model, ".csv"), row.names=F)
oneDaysTempYrTab0p5mm <- do.call(rbind.data.frame, wetDaysByYr0p5mm)
write.csv(oneDaysTempYrTab0p5mm, paste0(dataDir, dataNm, "countyMACA_0p5mmwetDaysByYr_", model, ".csv"), row.names=F)

# 99th Thres ------------------------------------------------------------------
dS = c(1990, 2020, 2050)
dE = c(2019, 2049, 2079)
calcWet99thThres <- lapply(prFiles, calc99thThreshold, decsStart=dS, decsEnd=dE, 
                           type="pr", fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T, 
                           trueDate=T)
one99th <- do.call(rbind.data.frame, calcWet99thThres)
write.csv(one99th, paste0(dataDir, dataNm, "countyMACA_99thThres_", model, ".csv"), row.names=F)

# 99th Days ------------------------------------------------------------------
calcWet99thDays <- lapply(prFiles, calcThresNumDays, decs=climDecs, 
                          thres="99th", type="99thpr", 
                          fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T, 
                          trueDate=T)
chkWetResults99th <- sapply(calcWet99thDays, length)
if(length(which(chkWetResults99th!=2))==0){
  gridvals99th <- (1:length(prFiles))
} else { gridvals99th <- (1:length(prFiles))[-which(chkWetResults99th!=2)] }

wetSummaries99th <- lapply(gridvals99th, function(chd){calcWet99thDays[[chd]][[1]]})
wetByYr99th <- lapply(gridvals99th, function(chd){calcWet99thDays[[chd]][[2]]})
oneTempTab99th <- do.call(rbind.data.frame, wetSummaries99th)
write.csv(oneTempTab99th, paste0(dataDir, dataNm, "countyMACA_99thwetDays_", model, ".csv"), row.names=F)
oneTempYrTab99th <- do.call(rbind.data.frame, wetByYr99th)
write.csv(oneTempYrTab99th, paste0(dataDir, dataNm, "countyMACA_99thwetDaysByYr_", model, ".csv"), row.names=F)

# Total precip 99th ------------------------------------------------------------
calcWetTot99th <- lapply(prFiles, calcThresWetTot, decs=climDecs, thres="99th",
                     base =NA, type="tot_thres", fileNMSplt1=".1950",
                     fileNMSplt2="grid.", inRCP=T, trueDate=T)
chkTotWetResults99th <- sapply(calcWetTot99th, length)
if(length(which(chkTotWetResults99th!=2))==0){
  gridvalsTot99th <- (1:length(prFiles))
} else { gridvalsTot99th <- (1:length(prFiles))[-which(chkTotWetResults99th!=2)] }

wetTotSummaries99th <- lapply(gridvalsTot99th, function(chd){calcWetTot99th[[chd]][[1]]})
wetTotByYr99th <- lapply(gridvalsTot99th, function(chd){calcWetTot99th[[chd]][[2]]})
oneTotTempTab99th <- do.call(rbind.data.frame, wetTotSummaries99th)
write.csv(oneTotTempTab99th, paste0(dataDir, dataNm, "countyMACA_99thtot_", model, ".csv"), row.names=F)
oneTotTempYrTab99th <- do.call(rbind.data.frame, wetTotByYr99th)
write.csv(oneTotTempYrTab99th, paste0(dataDir, dataNm, "countyMACA_99thtotByYr_", model, ".csv"), row.names=F)

# 2020 --------------------------------------------------------------------

# ##########################################################################
# 
# # # Seasonal Cycle --------------------------------------------------------
# calcCycle <- lapply(prFiles, calcSeasonCycle, decs=centDecs, thres=NULL, type="tot",
#                     fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                     trueDate=T)
# 
# chkWetResultsCycle <- sapply(calcCycle, length)
# if(length(which(chkWetResultsCycle!=2))==0){
#   gridvalsCycle <- (1:length(prFiles))
# } else { gridvalsCycle <- (1:length(prFiles))[-which(chkWetResultsCycle!=2)] }
# 
# wetCycle1950 <- lapply(gridvalsCycle, function(chd){calcCycle[[chd]][[1]]})
# wetCycle2020 <- lapply(gridvalsCycle, function(chd){calcCycle[[chd]][[2]]})
# oneTempCycle1950 <- do.call(rbind.data.frame, wetCycle1950)
# oneTempCycle1950 <- apply(oneTempCycle1950,2,as.character) #flatten the list
# write.csv(oneTempCycle1950, paste0(outDir, dataNm, "countyMACA_start_tot_cent", centDecs[1], "_", model, ".csv"), row.names=F)
# oneTempCycle2020 <- do.call(rbind.data.frame, wetCycle2020)
# oneTempCycle2020 <- apply(oneTempCycle2020,2,as.character) #flatten the list
# write.csv(oneTempCycle2020, paste0(outDir, dataNm, "countyMACA_start_tot_cent", centDecs[2], "_", model, ".csv"), row.names=F)
# 
# print("cycle done!")
# 
# ##########################################################################
# # Seasonal Cycle Extremes --------------------------------------------------------
# calcCycleExt <- lapply(prFiles, calcSeasonCycle, decs=centDecs, 
#                        thres=conv_unit(2.0,"inch","mm"), type="extdays", 
#                        fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                        trueDate=T)
# 
# chkWetResultsCycleExt <- sapply(calcCycleExt, length)
# if(length(which(chkWetResultsCycleExt!=2))==0){
#   gridvalsCycleExt <- (1:length(prFiles))
# } else { gridvalsCycleExt <- (1:length(prFiles))[-which(chkWetResultsCycleExt!=2)] }
# 
# wetCycle1950Ext <- lapply(gridvalsCycleExt, function(chd){calcCycleExt[[chd]][[1]]})
# wetCycle2020Ext <- lapply(gridvalsCycleExt, function(chd){calcCycleExt[[chd]][[2]]})
# oneTempCycle1950Ext <- do.call(rbind.data.frame, wetCycle1950Ext)
# oneTempCycle1950Ext <- apply(oneTempCycle1950Ext,2,as.character) #flatten the list
# write.csv(oneTempCycle1950Ext, paste0(outDir, dataNm, "countyMACA_start_2in_cent", centDecs[1], "_", model, ".csv"), row.names=F)
# oneTempCycle2020Ext <- do.call(rbind.data.frame, wetCycle2020Ext)
# oneTempCycle2020Ext <- apply(oneTempCycle2020Ext,2,as.character) #flatten the list
# write.csv(oneTempCycle2020Ext, paste0(outDir, dataNm, "countyMACA_start_2in_cent", centDecs[2], "_", model, ".csv"), row.names=F)
# 
# print("Ext cycle done!")

##########################################################################
# IDF Change Factor --------------------------------------------------------
#calcChange <- lapply(prFiles, calcChangeFactor, decs=centDecs, type="pr",
#                     fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                     trueDate=T)
#length(calcChange)
#chkWetResultsChange <- sapply(calcChange, length)
#if(length(which(chkWetResultsChange!=2))==0){
#  gridvalsChange <- (1:length(prFiles))
#} else { gridvalsChange <- (1:length(prFiles))[-which(chkWetResultsChange!=2)] }
#
#wetChangeFactor <- lapply(gridvalsChange, function(chd){calcChange[[chd]][[1]]})
#wetGEV <- lapply(gridvalsChange, function(chd){calcChange[[chd]][[2]]})
#oneTempTab2in <- do.call(rbind.data.frame, wetChangeFactor)
#write.csv(oneTempTab2in, paste0(outDir, dataNm, "countyMACA_changeFactor_cent_", model, ".csv"), row.names=F)
#oneTempYrTab2in <- do.call(rbind.data.frame, wetGEV)
#write.csv(oneTempYrTab2in, paste0(outDir, dataNm, "countyMACA_GEV_cent_", model, ".csv"), row.names=F)
#
#print("IDF done!")
#
##########################################################################
# 2050 --------------------------------------------------------------------

# # # Seasonal Cycle --------------------------------------------------------
# calcCycle <- lapply(prFiles, calcSeasonCycle, decs=futDecs, thres=NULL, type="tot",
#                     fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                     trueDate=T)
# 
# chkWetResultsCycle <- sapply(calcCycle, length)
# if(length(which(chkWetResultsCycle!=2))==0){
#   gridvalsCycle <- (1:length(prFiles))
# } else { gridvalsCycle <- (1:length(prFiles))[-which(chkWetResultsCycle!=2)] }
# 
# wetCycle1950 <- lapply(gridvalsCycle, function(chd){calcCycle[[chd]][[1]]})
# wetCycle2020 <- lapply(gridvalsCycle, function(chd){calcCycle[[chd]][[2]]})
# oneTempCycle1950 <- do.call(rbind.data.frame, wetCycle1950)
# oneTempCycle1950 <- apply(oneTempCycle1950,2,as.character) #flatten the list
# write.csv(oneTempCycle1950, paste0(outDir, dataNm, "countyMACA_start_tot_fut", futDecs[1], "_", model, ".csv"), row.names=F)
# oneTempCycle2020 <- do.call(rbind.data.frame, wetCycle2020)
# oneTempCycle2020 <- apply(oneTempCycle2020,2,as.character) #flatten the list
# write.csv(oneTempCycle2020, paste0(outDir, dataNm, "countyMACA_start_tot_fut", futDecs[2], "_", model, ".csv"), row.names=F)
# 
# print("cycle done!")
# 
# ##########################################################################
# # Seasonal Cycle Extremes --------------------------------------------------------
# calcCycleExt <- lapply(prFiles, calcSeasonCycle, decs=futDecs, 
#                        thres=conv_unit(2.0,"inch","mm"), type="extdays", 
#                        fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                        trueDate=T)
# 
# chkWetResultsCycleExt <- sapply(calcCycleExt, length)
# if(length(which(chkWetResultsCycleExt!=2))==0){
#   gridvalsCycleExt <- (1:length(prFiles))
# } else { gridvalsCycleExt <- (1:length(prFiles))[-which(chkWetResultsCycleExt!=2)] }
# 
# wetCycle1950Ext <- lapply(gridvalsCycleExt, function(chd){calcCycleExt[[chd]][[1]]})
# wetCycle2020Ext <- lapply(gridvalsCycleExt, function(chd){calcCycleExt[[chd]][[2]]})
# oneTempCycle1950Ext <- do.call(rbind.data.frame, wetCycle1950Ext)
# oneTempCycle1950Ext <- apply(oneTempCycle1950Ext,2,as.character) #flatten the list
# write.csv(oneTempCycle1950Ext, paste0(outDir, dataNm, "countyMACA_start_2in_fut", futDecs[1], "_", model, ".csv"), row.names=F)
# oneTempCycle2020Ext <- do.call(rbind.data.frame, wetCycle2020Ext)
# oneTempCycle2020Ext <- apply(oneTempCycle2020Ext,2,as.character) #flatten the list
# write.csv(oneTempCycle2020Ext, paste0(outDir, dataNm, "countyMACA_start_2in_fut", futDecs[2], "_", model, ".csv"), row.names=F)
# 
# print("Ext cycle done!")

##########################################################################
## IDF Change Factor --------------------------------------------------------
#calcChange <- lapply(prFiles, calcChangeFactor, decs=futDecs, type="pr",
#                     fileNMSplt1=".1950", fileNMSplt2="grid.", inRCP=T,
#                     trueDate=T)
#length(calcChange)
#chkWetResultsChange <- sapply(calcChange, length)
#if(length(which(chkWetResultsChange!=2))==0){
#  gridvalsChange <- (1:length(prFiles))
#} else { gridvalsChange <- (1:length(prFiles))[-which(chkWetResultsChange!=2)] }
#
#wetChangeFactor <- lapply(gridvalsChange, function(chd){calcChange[[chd]][[1]]})
#wetGEV <- lapply(gridvalsChange, function(chd){calcChange[[chd]][[2]]})
#oneTempTab2in <- do.call(rbind.data.frame, wetChangeFactor)
#write.csv(oneTempTab2in, paste0(outDir, dataNm, "countyMACA_changeFactor_fut_", model, ".csv"), row.names=F)
#oneTempYrTab2in <- do.call(rbind.data.frame, wetGEV)
#write.csv(oneTempYrTab2in, paste0(outDir, dataNm, "countyMACA_GEV_fut_", model, ".csv"), row.names=F)
#
#print("IDF done!")
