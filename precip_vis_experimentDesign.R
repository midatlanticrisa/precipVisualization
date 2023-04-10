##########################################################################
##########################################################################
## Script Name: precip_vis_locationsize.R
## Purpose of Script: This script creates the graphs used in the Outlook PDF
## files. The percip graphs are based on the extracted and summarized MACA 
## and GridMet data (done on ROAR). It is known that some visual 
## updates to the output graphics are needed.
##
## Special Requirements: Summarized precip data
##
## Author: Kelsey Ruckert
## Email: klr324@psu.edu
## Date: 11/30/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################

library(plyr)
library(dplyr)
library(zoo)
library(Cairo)
library(plotly)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(measurements)
# library(stringr)
# install.packages("vioplot")
# library("vioplot")
library(RColorBrewer)

##########################################################################
# Load the functions
##########################################################################
source("precip_functions.R")

##########################################################################
# Setup directories
##########################################################################
## Set to create figures or just create the csv file
create.plot = TRUE
# What location, size?
NHstabv = "mtsunapeeNH_100km"

NHplotDir = paste0("Users:klr324:Documents:Github:precipVisualization:plots:", NHstabv, ":")

NHgridDir <- paste0("TimeSeries_GridMet/", NHstabv, "/")
NHmacaDir <- paste0("TimeSeries_MACA/", NHstabv, "/")
NHprecipDir <- paste0("plots/", NHstabv, "/")

# Generate precipDir directories if they doesn't already exist
# script will error out otherwise
if(!dir.exists(NHprecipDir)){
  dir.create(NHprecipDir, recursive=TRUE)
}

##########################################################################
# Read in precipitation data files
##########################################################################
# 0.5 mm --------------------------------------------------------------------
NHwet0p5Ave <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_0p5mmwetDays_full.csv"))
NHwet0p5days <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_0p5mmwetDaysByYr_full.csv"))
NHwet0p5Obs <- read.csv(paste0(NHgridDir, NHstabv, "countyGridMet_0p5mmwetDaysByYr.csv"))

# 99th threshold --------------------------------------------------------------------
NHwet99thThres <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_99thThres_full.csv"))
NHwet99thObsThres <- read.csv(paste0(NHgridDir, NHstabv, "countyGridMet_99thThres.csv"))

# 99th wet days --------------------------------------------------------------------
NHwet99thAve <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_99thwetDays_full.csv"))
NHwet99thdays <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_99thwetDaysByYr_full.csv"))
NHwet99thObs <- read.csv(paste0(NHgridDir, NHstabv, "countyGridMet_99thwetDaysByYr.csv"))

# Annual total ------------------------------------------------------------
NHwettotAve <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_tot_full.csv"))
NHwettotdays <- read.csv(paste0(NHmacaDir, NHstabv, "CountyMACA_totByYr_full.csv"))
NHwettotObs <- read.csv(paste0(NHgridDir, NHstabv, "countyGridMet_totByYr.csv"))

##########################################################################
# Current information
##########################################################################

NHcurrent_total_precip <- mean(NHwettotObs$wetDays[which(NHwettotObs$year %in% 1990:2019)])/25.4
print(paste("Average annual precipitation is", NHcurrent_total_precip, "inches", "in", NHstabv))

NHcurrent_day_precip <- mean(NHwet0p5Obs$wetDays[which(NHwet0p5Obs$year %in% 1990:2019)])
print(paste("Average number of wet days per year is", NHcurrent_day_precip, "in", NHstabv))

NHcurrent_99th_precip <- mean(NHwet99thObs$wetDays[which(NHwet99thObs$year %in% 1990:2019)])
print(paste("The 1% event precipitation is", (NHwet99thObsThres$thres)/25.4, "inches (1990-2019)", "in", NHstabv))
print(paste("Average number of days per year ≥ 1% event is", NHcurrent_99th_precip, "in", NHstabv))

##########################################################################
# Create figures
##########################################################################

##########################################################################
# Precipitation - as Polygon graph
##########################################################################
# NEW HAMPSHIRE
# 0.5 mm --------------------------------------------------------------------
NHpoly0p5 = calcPrecipRunAveGraph("shp", dataAveTab = NHwet0p5Ave, dataTab=NHwet0p5days,
                                obsTab = NHwet0p5Obs, ylabel = "Number of days with precipitation",
                                var="0p5", outDir=NHprecipDir, create.plot=create.plot)

# 99th wet days --------------------------------------------------------------------
NHpoly99th = calcPrecipRunAveGraph("shp", dataAveTab = NHwet99thAve, dataTab=NHwet99thdays,
                                obsTab = NHwet99thObs, ylabel = "Number of days with\n1% precipitation event (days/year)",
                                var="99thdays", outDir=NHprecipDir, create.plot=create.plot, 
                                add.legend = TRUE)

NHpoly99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
                                outDir = NHprecipDir, 
                var = "99th", ylabel="Amount of precipitation during\n1% event (inches)", plot_type = "poly", 
                create.plot=TRUE, leg.inside = FALSE, add.legend = TRUE)

##########################################################################
# Precipitation Change Bar Graph
##########################################################################
# NEW HAMPSHIRE
NHbar0p5 = calcPrecipChangeBarGraph("shp", dataTab=NHwet0p5days,  
                                  ylabel = "Percent change in days with precipitation", 
                                  var="0p5",outDir=NHprecipDir, create.plot=create.plot)

NHbar99th = calcPrecipObsBarGraph("shp", dataTab=NHwet99thdays,obsTab = NHwet99thObs,
                               ylabel = "Average number of days with\n1% precipitation event (days/year)",
                               var="99thdays", outDir=NHprecipDir, create.plot=create.plot, 
                               add.legend = TRUE, leg.inside=FALSE)

NHbar99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
                               outDir = NHprecipDir, 
                               var = "99th", ylabel="Average amount of precipitation\nduring 1% event (inches)", plot_type = "bar", 
                               create.plot=TRUE, leg.inside = TRUE, add.legend = TRUE)

##########################################################################
# Precipitation Box Graph
theme_set(theme_bw())
##########################################################################
# NEW HAMPSHIRE
NHbox0p5 = calcPrecipBox("shp", dataTab=NHwet0p5days, obsTab = NHwet0p5Obs,
              var="0p5", outDir=NHprecipDir, 
              ylabel = "Number of days with precipitation",
              create.plot=TRUE, add.legend = TRUE)

NHbox99th = calcPrecipBox("shp", dataTab=NHwet99thdays, obsTab = NHwet99thObs, 
                                ylabel = "Number of days with\n1% precipitation event (days/year)",
                                var="99thdays", outDir=NHprecipDir, 
                                create.plot=create.plot, add.legend = TRUE)

NHbox99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres,
                                  outDir = NHprecipDir, var = "99th", 
                               ylabel="Amount of precipitation during\n1% event (inches)",
                                  plot_type = "box", create.plot=TRUE, leg.inside = FALSE,
                                  add.legend = TRUE)

##########################################################################
# Outlooks
##########################################################################
cnames = c("var", "heavy-event", "heavy-days", "value", "high-likelihood", 
           "low-likelihood", "high-fut-vals", "low-fut-vals", "@fig")

# Polygon graphs ----------------------------------------------------------
locationAEventpoly = data.frame(var = "Intensity", heavyevent = round(NHpoly99thres$obsmean,1), 
                           heavyday = round(NHpoly99th$obsmean,1), val = "1% event",
                           likelihood85 = single.likl(NHpoly99thres$likl.increase85, "intensify"),
                           likelihood45 = single.likl(NHpoly99thres$likl.increase45, "intensify"),
                           futpoly85 = single.change(NHpoly99thres$percentobsincrease85, NHpoly99thres$rcp85_20502079, "inches"),
                           futpoly45 = single.change(NHpoly99thres$percentobsincrease45, NHpoly99thres$rcp45_20502079, "inches"),
                           polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-percpolygraph1.eps"))
colnames(locationAEventpoly) <- cnames

locationADaypoly = data.frame(var = "Frequency", heavyevent = round(NHpoly99thres$obsmean,1),
                              heavyday = round(NHpoly99th$obsmean,1), val = "number of days with a 1% event", 
                           likelihood85 = single.likl(NHpoly99th$likl.increase85, "increase"),
                           likelihood45 = single.likl(NHpoly99th$likl.increase45, "increase"),
                           futpoly85 = single.change(NHpoly99th$percentobsincrease85, NHpoly99th$rcp85_20502079, "days per year"),
                           futpoly45 = single.change(NHpoly99th$percentobsincrease45, NHpoly99th$rcp45_20502079, "days per year"),
                           polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipgraph1Proj.eps"))
colnames(locationADaypoly) <- cnames

# Bar graphs ----------------------------------------------------------
locationAEventbar = data.frame(var = "Intensity", eventpoly = round(NHbar99thres$hindcastmean,1),
                          daypoly = round(NHbar99th$baseline,1), val = "1% event",
                          likelihood85 = single.likl(NHbar99thres$likl.increase85, "intensify"),
                          likelihood45 = single.likl(NHbar99thres$likl.increase45, "intensify"),
                          futpoly85 = single.change(NHbar99thres$percentobsincrease85, NHbar99thres$rcp85_20502079, "inches"),
                          futpoly45 = single.change(NHbar99thres$percentobsincrease45, NHbar99thres$rcp45_20502079, "inches"),
                          polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-obsbargraph1.eps"))
colnames(locationAEventbar) <- cnames

locationADaybar = data.frame(var = "Frequency", eventpoly = round(NHbar99thres$hindcastmean,1),
                          daypoly = round(NHbar99th$baseline,1), val = "number of days with a 1% event",
                          likelihood85 = single.likl(NHbar99th$likl.increase85, "increase"),
                          likelihood45 = single.likl(NHbar99th$likl.increase45, "increase"),
                          futpoly85 = single.change(NHbar99th$percentobsincrease85, NHbar99th$rcp85_20502079, "days per year"),
                          futpoly45 = single.change(NHbar99th$percentobsincrease45, NHbar99th$rcp45_20502079, "days per year"),
                          polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-ObsprecipBargraph1.eps"))
colnames(locationADaybar) <- cnames

# Box graphs ----------------------------------------------------------
locationAEventbox = data.frame(var = "Intensity", eventpoly = round(NHbox99thres$obsmean,1),
                          daypoly = round(NHbox99th$obsmean,1), val = "1% event",
                          likelihood85 = single.likl(NHbox99thres$likl.increase85, "intensify"),
                          likelihood45 = single.likl(NHbox99thres$likl.increase45, "intensify"),
                          futpoly85 = single.change(NHbox99thres$percentobsincrease85, NHbox99thres$rcp85_20502079, "inches"),
                          futpoly45 = single.change(NHbox99thres$percentobsincrease45, NHbox99thres$rcp45_20502079, "inches"),
                          polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-Boxgraph1.eps"))
colnames(locationAEventbox) <- cnames

locationADaybox = data.frame(var = "Frequency", eventpoly = round(NHbox99thres$obsmean,1),
                          daypoly = round(NHbox99th$obsmean,1), val = "number of days with a 1% event",
                          likelihood85 = single.likl(NHbox99th$likl.increase85, "increase"),
                          likelihood45 = single.likl(NHbox99th$likl.increase45, "increase"),
                          futpoly85 = single.change(NHbox99th$percentobsincrease85, NHbox99th$rcp85_20502079, "days per year"),
                          futpoly45 = single.change(NHbox99th$percentobsincrease45, NHbox99th$rcp45_20502079, "days per year"),
                          polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipBoxgraph1.eps"))
colnames(locationADaybox) <- cnames

outlooks = rbind(locationAEventpoly, locationADaypoly, locationAEventbar, 
                 locationADaybar, locationAEventbox, locationADaybox)
encodeConnection <- file("outlooks/outlooks_merge.txt",encoding = "UTF-16")
write.table(outlooks, file=encodeConnection, row.names=FALSE, sep = "\t")

##########################################################################
# END
##########################################################################

