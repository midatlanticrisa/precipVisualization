##########################################################################
##########################################################################
## Script Name: organizeMACA.R
## Purpose of Script: Script that organizes all the MACA output timeseries into
## one table.
##
## Special Requirements: A series of csv files with MACA time series data
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

dataDir <- paste("/base/TimeSeries_MACA/", dataDirNm, sep="")

##########################################################################
# PLACEHOLDER FOR NOW
##########################################################################
# wetUncompleteFiles <- list.files(dataDir, pattern="notCompleteWet", full.names=T)
# hotUncompleteFiles <- list.files(dataDir, pattern="notCompleteHot", full.names=T)
# mdCleanPrFiles <- c()
# tMaxFiles <- c()
# sapply(wetUncompleteFiles, function(ldFile){varName<-load(ldFile);
#                                             mdCleanPrFiles<<-c(mdCleanPrFiles,get(varName))})
# sapply(hotUncompleteFiles, function(ldFile){varName<-load(ldFile);
#                                             tMaxFiles<<-c(tMaxFiles,get(varName))})
# 
# save("notCompleteWet", file=paste0(dataDir, dataNm, "notCompleteDaysWetInd2in_", model, ".RData"))
# save("notCompleteDaysWet", file=paste0(dataDir, dataNm, "notCompleteDaysWetInd1in_", model, ".RData"))
# save("notCompleteWet", file=paste0(dataDir, dataNm, "notCompleteWetIndtot",
#                                    gsub("\\.", "p", thres),"_", model, ".RData"))
# save("notCompleteTotWet", file=paste0(dataDir, dataNm, "notCompleteTotWetInd_", model, ".RData"))
# save("notCompleteHot", file=paste0(dataDir, dataNm, "notCompleteHotInd_", model, ".RData"))
# save("notCompletefrost", file=paste0(dataDir, dataNm, "notCompletefrostInd_", model, ".RData"))
# save("notCompletegsl", file=paste0(dataDir, dataNm, "notCompletegslInd_", model, ".RData"))

##########################################################################
# FUNCTION TO ORGANIZE DATA INTO ONE TABLE
##########################################################################
organize.table <- function(dataDir, dataNm, var, byYear=FALSE, justTotal=FALSE){
  # organize the Climate Stats into one table
  pat = paste0("_", var, "_")
  files <- list.files(dataDir, pat, full.names=T)
  
  if(justTotal){
    # Select only the total files
    totind <- grep(paste0("countyMACA", pat), files)
    if(!identical(totind, integer(0))){ files <- files[totind]}
  }

  if(!byYear){
    # Remove files that have ByYr
    byind <- grep("ByYr", files)
    if(!identical(byind, integer(0))){ files <- files[-byind]}
  }

  # Remove files that have full. Just to make sure we aren't reading any previously
  # organized tables
  fullind <- grep("full", files)
  if(!identical(fullind, integer(0))){ files <- files[-fullind]}
  
  # Read and format files
  dataList <- lapply(files, read.csv)
  oneTab <- do.call(rbind.data.frame, dataList)
  
  write.csv(oneTab, paste0(dataDir, dataNm, "CountyMACA_", var, "_full.csv"), row.names=F)
}
##########################################################################
# RUN THE FUNCTION
##########################################################################
# # organize the yearly climate stats into one table

# 2in
organize.table(dataDir, dataNm, "2inwetDays")
organize.table(dataDir, dataNm, "2inwetDaysByYr", byYear=TRUE)

# 1in
organize.table(dataDir, dataNm, "1inwetDays")
organize.table(dataDir, dataNm, "1inwetDaysByYr", byYear=TRUE)

# total
organize.table(dataDir, dataNm, "tot", justTotal=TRUE)
organize.table(dataDir, dataNm, "totByYr", byYear=TRUE)

# total 99th
organize.table(dataDir, dataNm, "99thtot")
organize.table(dataDir, dataNm, "99thtotByYr", byYear=TRUE)

# 2020 - 2069
organize.table(dataDir, dataNm, "changeFactor_cent")
organize.table(dataDir, dataNm, "GEV_cent")
organize.table(dataDir, dataNm, "tot_cent1950")
organize.table(dataDir, dataNm, "tot_cent2020")
organize.table(dataDir, dataNm, "2in_cent1950")
organize.table(dataDir, dataNm, "2in_cent2020")

# 2050 - 2099
organize.table(dataDir, dataNm, "changeFactor_fut")
organize.table(dataDir, dataNm, "GEV_fut")
organize.table(dataDir, dataNm, "tot_fut1950")
organize.table(dataDir, dataNm, "tot_fut2050")
organize.table(dataDir, dataNm, "2in_fut1950")
organize.table(dataDir, dataNm, "2in_fut2050")
##########################################################################
##########################################################################
