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
#library(export)
library(measurements)
library(stringr)
# install.packages("vioplot")
library("vioplot")
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
CAstabv = "wascoCA_100km"

NHlocation = "Location A"
CAlocation = "Location B"

NHplotDir = paste0("Users:klr324:Documents:Github:precipVisualization:plots:", NHstabv, ":")
CAplotDir = paste0("Users:klr324:Documents:Github:precipVisualization:plots:", CAstabv, ":")

NHgridDir <- paste0("TimeSeries_GridMet/", NHstabv, "/")
NHmacaDir <- paste0("TimeSeries_MACA/", NHstabv, "/")
NHprecipDir <- paste0("plots/", NHstabv, "/")

CAgridDir <- paste0("TimeSeries_GridMet/", CAstabv, "/")
CAmacaDir <- paste0("TimeSeries_MACA/", CAstabv, "/")
CAprecipDir <- paste0("plots/", CAstabv, "/")

# Generate precipDir directories if they doesn't already exist
# script will error out otherwise
if(!dir.exists(NHprecipDir)){
  dir.create(NHprecipDir, recursive=TRUE)
}
if(!dir.exists(CAprecipDir)){
  dir.create(CAprecipDir, recursive=TRUE)
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

# 0.5 mm --------------------------------------------------------------------
CAwet0p5Ave <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_0p5mmwetDays_full.csv"))
CAwet0p5days <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_0p5mmwetDaysByYr_full.csv"))
CAwet0p5Obs <- read.csv(paste0(CAgridDir, CAstabv, "countyGridMet_0p5mmwetDaysByYr.csv"))

# 99th threshold --------------------------------------------------------------------
CAwet99thThres <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_99thThres_full.csv"))
CAwet99thObsThres <- read.csv(paste0(CAgridDir, CAstabv, "countyGridMet_99thThres.csv"))

# 99th wet days --------------------------------------------------------------------
CAwet99thAve <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_99thwetDays_full.csv"))
CAwet99thdays <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_99thwetDaysByYr_full.csv"))
CAwet99thObs <- read.csv(paste0(CAgridDir, CAstabv, "countyGridMet_99thwetDaysByYr.csv"))

# Annual total ------------------------------------------------------------
CAwettotAve <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_tot_full.csv"))
CAwettotdays <- read.csv(paste0(CAmacaDir, CAstabv, "CountyMACA_totByYr_full.csv"))
CAwettotObs <- read.csv(paste0(CAgridDir, CAstabv, "countyGridMet_totByYr.csv"))

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

CAcurrent_total_precip <- mean(CAwettotObs$wetDays[which(CAwettotObs$year %in% 1990:2019)])/25.4
print(paste("Average annual precipitation is", CAcurrent_total_precip, "inches", "in", CAstabv))

CAcurrent_day_precip <- mean(CAwet0p5Obs$wetDays[which(CAwet0p5Obs$year %in% 1990:2019)])
print(paste("Average number of wet days per year is", CAcurrent_day_precip, "in", CAstabv))

CAcurrent_99th_precip <- mean(CAwet99thObs$wetDays[which(CAwet99thObs$year %in% 1990:2019)])
print(paste("The 1% event precipitation is", (CAwet99thObsThres$thres)/25.4, "inches (1990-2019)", "in", CAstabv))
print(paste("Average number of days per year ≥ 1% event is", CAcurrent_99th_precip, "in", CAstabv))

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

NHpolytot = calcPrecipRunAveGraph("shp", dataAveTab = NHwettotAve, dataTab=NHwettotdays,
                                obsTab = NHwettotObs, ylabel = "Annual total precipitation (in)",
                                var="tot", outDir=NHprecipDir, create.plot=create.plot)

NHpoly99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
                                outDir = NHprecipDir, 
                var = "99th", ylabel="Amount of precipitation during\n1% event (inches)", plot_type = "poly", 
                create.plot=TRUE, leg.inside = FALSE, add.legend = TRUE)
# CALIFORNIA
# 0.5 mm --------------------------------------------------------------------
CApoly0p5 = calcPrecipRunAveGraph("shp", dataAveTab = CAwet0p5Ave, dataTab=CAwet0p5days,
                                      obsTab = CAwet0p5Obs, ylabel = "Number of days with precipitation",
                                      var="0p5", outDir=CAprecipDir, create.plot=create.plot)

# 99th wet days --------------------------------------------------------------------
CApoly99th = calcPrecipRunAveGraph("shp", dataAveTab = CAwet99thAve, dataTab=CAwet99thdays,
                                       obsTab = CAwet99thObs, ylabel = "Number of days with\n1% precipitation event (days/year)",
                                       var="99thdays", outDir=CAprecipDir, create.plot=create.plot, add.legend = TRUE)

CApolytot = calcPrecipRunAveGraph("shp", dataAveTab = CAwettotAve, dataTab=CAwettotdays,
                                      obsTab = CAwettotObs, ylabel = "Annual total precipitation (in)",
                                      var="tot", outDir=CAprecipDir, create.plot=create.plot)

CApoly99thres = calcPrecipThres("shp", dataTab = CAwet99thThres, obsTab = CAwet99thObsThres, 
                                outDir = CAprecipDir, 
                                    var = "99th", ylabel="Amount of precipitation during\n1% event (inches)", plot_type = "poly", 
                                    create.plot=TRUE, leg.inside = FALSE, add.legend = TRUE)

##########################################################################
# Precipitation Change Bar Graph
##########################################################################
# NEW HAMPSHIRE
NHbar0p5 = calcPrecipChangeBarGraph("shp", dataTab=NHwet0p5days,  
                                  ylabel = "Percent change in days with precipitation", 
                                  var="0p5",outDir=NHprecipDir, create.plot=create.plot)

# NHbar99th = calcPrecipPercentChangeBarGraph("shp", dataTab=NHwet99thdays,
#                                  ylabel = "Percent change in the average number\nof days with 1% precipitation event",
#                                  var="99thdays", outDir=NHprecipDir, create.plot=create.plot, 
#                                  add.legend = TRUE)
# 
# NHbar99th = calcPrecipChangeBarGraph("shp", dataTab=NHwet99thdays,
#                                             ylabel = "Change in the average number\nof days with 1% precipitation event",
#                                             var="99thdaysChange", outDir=NHprecipDir, create.plot=create.plot, 
#                                             add.legend = TRUE)

NHbar99th = calcPrecipObsBarGraph("shp", dataTab=NHwet99thdays,obsTab = NHwet99thObs,
                               ylabel = "Average number of days with\n1% precipitation event (days/year)",
                               var="99thdays", outDir=NHprecipDir, create.plot=create.plot, 
                               add.legend = TRUE, leg.inside=FALSE)

# NHbar99th = calcPrecipBarGraph("shp", dataTab=NHwet99thdays,
#                                      ylabel = "Average number of days with\n1% precipitation event (days/year)",
#                                      var="99thdays", outDir=NHprecipDir, create.plot=create.plot, 
#                                      add.legend = TRUE)

NHbartot = calcPrecipChangeBarGraph("shp", dataTab=NHwettotdays,  
                                  ylabel = "Percent change in annual total precipitation", 
                                  var="tot",outDir=NHprecipDir, create.plot=create.plot)

# NHbar99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
#                                outDir = NHprecipDir, 
#                 var = "99th", ylabel="Percent change in the average amount\nof precipitation during 1% event", plot_type = "bar", 
#                 create.plot=TRUE, leg.inside = TRUE, add.legend = TRUE)

NHbar99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
                               outDir = NHprecipDir, 
                               var = "99th", ylabel="Average amount of precipitation\nduring 1% event (inches)", plot_type = "bar", 
                               create.plot=TRUE, leg.inside = TRUE, add.legend = TRUE)

# CALIFORNIA
CAbar0p5 = calcPrecipChangeBarGraph("shp", dataTab=CAwet0p5days, leg.inside = FALSE,
                                        ylabel = "Percent change in days with precipitation", 
                                        var="0p5",outDir=CAprecipDir, create.plot=create.plot)

CAbar99th = calcPrecipChangeBarGraph("shp", dataTab=CAwet99thdays,
                                         ylabel = "Percent change in the average number\nof days with 1% precipitation event",
                                         var="99thdays", outDir=CAprecipDir, create.plot=create.plot, add.legend = TRUE)

CAbartot = calcPrecipChangeBarGraph("shp", dataTab=CAwettotdays,  
                                        ylabel = "Percent change in annual total precipitation", 
                                        var="tot",outDir=CAprecipDir, create.plot=create.plot)

CAbar99thres = calcPrecipThres("shp", dataTab = CAwet99thThres, obsTab = CAwet99thObsThres, 
                               outDir = CAprecipDir, 
                                   var = "99th", ylabel="Percent change in the average amount\nof precipitation during 1% event", plot_type = "bar", 
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

NHboxtot = calcPrecipBox("shp", dataTab=NHwettotdays, obsTab = NHwettotObs,
                               ylabel = "Annual total precipitation (in)",
                               var="tot", outDir=NHprecipDir,
                               create.plot=TRUE)

NHbox99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres,
                                  outDir = NHprecipDir, var = "99th", 
                               ylabel="Amount of precipitation during\n1% event (inches)",
                                  plot_type = "box", create.plot=TRUE, leg.inside = FALSE,
                                  add.legend = TRUE)

CAbox0p5 = calcPrecipBox("shp", dataTab=CAwet0p5days, obsTab = CAwet0p5Obs,
                               ylabel = "Number of days with precipitation",
                               var="0p5", outDir=CAprecipDir,
                               create.plot=TRUE)

CAbox99th = calcPrecipBox("shp", dataTab=CAwet99thdays, obsTab = CAwet99thObs, 
                                ylabel = "Number of days with\n1% precipitation event (days/year)",
                                var="99thdays", outDir=CAprecipDir, 
                                create.plot=create.plot, add.legend = TRUE)

CAboxtot = calcPrecipBox("shp", dataTab=CAwettotdays, obsTab = CAwettotObs,
                               ylabel = "Annual total precipitation (in)",
                               var="tot", outDir=CAprecipDir,
                               create.plot=TRUE)

CAbox99thres = calcPrecipThres("shp", dataTab = CAwet99thThres, obsTab = CAwet99thObsThres,
                                  outDir = CAprecipDir, var = "99th", 
                               ylabel="Amount of precipitation during\n1% event (inches)",
                                  plot_type = "box", create.plot=TRUE, leg.inside = FALSE,
                                  add.legend = TRUE)

# ##########################################################################
# # Precipitation Violin Graph
# ##########################################################################
# NHviolin0p5 = calcPrecipViolin("shp", dataTab=NHwet0p5days, obsTab = NHwet0p5Obs,
#                              ylabel = "Number of days with precipitation",
#                              var="0p5", percent=FALSE, outDir=NHprecipDir,
#                              create.plot=TRUE, leg.inside = FALSE)
# 
# NHviolin99th = calcPrecipViolin("shp", dataTab=NHwet99thdays, obsTab = NHwet99thObs, 
#                               ylabel = "Number of 1% precipitation days",
#                               var="99thdays", outDir=NHprecipDir, 
#                               create.plot=create.plot, leg.inside = FALSE, 
#                               add.legend = FALSE)
# 
# NHviolintot = calcPrecipViolin("shp", dataTab=NHwettotdays, obsTab = NHwettotObs,
#                              ylabel = "Annual total precipitation (in)",
#                              var="tot", percent=FALSE, outDir=NHprecipDir,
#                              create.plot=TRUE, leg.inside = FALSE)
# 
# NHviolin99thres = calcPrecipThres("shp", dataTab = NHwet99thThres, obsTab = NHwet99thObsThres, 
#                                 outDir = NHprecipDir, var = "99th", ylabel="1% precipitation event (in)", 
#                                 plot_type = "vio", create.plot=TRUE, leg.inside = FALSE, 
#                                 add.legend = TRUE)
# 
# CAviolin0p5 = calcPrecipViolin("shp", dataTab=CAwet0p5days, obsTab = CAwet0p5Obs,
#                                    ylabel = "Number of days with precipitation",
#                                    var="0p5", percent=FALSE, outDir=CAprecipDir,
#                                    create.plot=TRUE, leg.inside = FALSE)
# 
# CAviolin99th = calcPrecipViolin("shp", dataTab=CAwet99thdays, obsTab = CAwet99thObs, 
#                                     ylabel = "Number of 1% precipitation days",
#                                     var="99thdays", outDir=CAprecipDir, 
#                                     create.plot=create.plot, leg.inside = FALSE, add.legend = FALSE)
# 
# CAviolintot = calcPrecipViolin("shp", dataTab=CAwettotdays, obsTab = CAwettotObs,
#                                    ylabel = "Annual total precipitation (in)",
#                                    var="tot", percent=FALSE, outDir=CAprecipDir,
#                                    create.plot=TRUE, leg.inside = FALSE)
# 
# CAviolin99thres = calcPrecipThres("shp", dataTab = CAwet99thThres, obsTab = CAwet99thObsThres, 
#                                       outDir = CAprecipDir, var = "99th", ylabel="1% precipitation event (in)", 
#                                       plot_type = "vio", create.plot=TRUE, leg.inside = FALSE, 
#                                       add.legend = TRUE)

##########################################################################
# Outlooks
##########################################################################
# cnames = c("location", "annual-rainfall", "annual-days", "heavy-event", 
#            "heavy-days", "fut-event", "fut-days", "percent-increase", 
#            "percent-days", "@fig", "@daysfig", "caption")

barcapEvent = paste("The graph shows the average change in the amount of precipitation received during",
                    "a 1% precipitation event compared to the average between 1990 and 2019.",  
               "The gray bars show the hindcast, (historic model results). Two scenarios of the future are shown",
               "as a high emissions scenario (RCP 8.5) in red and a low emissions scenario",
               "(RCP 4.5) in blue. Data retrieved from Multivariate Adaptive Constructed Analogs (MACA).")

polyviocapEvent = paste0("The graph shows the amount of precipitation received during", 
                    " a 1% precipitation event. Black dots represent observations and",
                    " the gray shading shows the hindcast (historic model results).",
                    " Two scenarios of the future are shown as a high emissions scenario (RCP 8.5) in red",
                    " and a low emissions scenario (RCP 4.5) in blue. Data",
                    " retrieved from Multivariate Adaptive Constructed Analogs (MACA) and",
                    " Gridded Surface Meteorological Dataset (gridMET).")

barcapDay = paste("The graph shows the average number of days in a year with a 1% precipitation event",
                  "compared to the average between 1990 and 2019. The gray bars show the",  
               "hindcast, (historic model results). Two scenarios of the future are shown",
               "as a high emissions scenario (RCP 8.5) in red and a low emissions scenario",
               "(RCP 4.5) in blue. Data retrieved from Multivariate Adaptive Constructed Analogs (MACA).")

polyviocapDay = paste0("The graph shows the number of days in a year with a 1% precipitation event", 
                    " . Black dots represent observations and",
                    " the gray shading shows the hindcast (historic model results).",
                    " Two scenarios of the future are shown as a high emissions scenario (RCP 8.5) in red",
                    " and a low emissions scenario (RCP 4.5) in blue. Data",
                    " retrieved from Multivariate Adaptive Constructed Analogs (MACA) and",
                    " Gridded Surface Meteorological Dataset (gridMET).")

# Polygon graphs ----------------------------------------------------------
# cnames = c("location", "annual-rainfall", "annual-days", "heavy-event", 
#            "heavy-days", "value", "likelihood", "fut-vals", "@fig", "caption")

cnames = c("var", "heavy-event", "heavy-days", "value", "high-likelihood", 
           "high-ave-change", "high-fut-vals", "low-likelihood", 
           "low-ave-change", "low-fut-vals", "@fig")

locationAEventpoly = data.frame(location = NHlocation, obs=round(NHcurrent_total_precip,1),
                           dayobs = round(NHcurrent_day_precip), eventpoly = round(NHpoly99thres$obsmean,1), 
                           daypoly = round(NHpoly99th$obsmean,1), val = "1% event",
                           likelihood = double.likl(NHpoly99thres$likl.increase45, NHpoly99thres$likl.increase85, "intensify"),
                           futpoly = double.change(NHpoly99thres$percentobsincrease45, NHpoly99thres$percentobsincrease85,
                                                   NHpoly99thres$rcp45_20502079, NHpoly99thres$rcp85_20502079, "inches"),
                           polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-polygraph1.eps"),
                           polycaption = polyviocapEvent)
colnames(locationAEventpoly) <- cnames

locationADaypoly = data.frame(location = NHlocation, obs=round(NHcurrent_total_precip,1),
                           dayobs = round(NHcurrent_day_precip), eventpoly = round(NHpoly99thres$obsmean,1),
                           daypoly = round(NHpoly99th$obsmean,1), val = "number of days with a 1% event", 
                           likelihood = double.likl(NHpoly99th$likl.increase45, NHpoly99th$likl.increase85, "increase"),
                           futpoly = double.change(NHpoly99th$percentobsincrease45, NHpoly99th$percentobsincrease, 
                                                   NHpoly99th$rcp45_20502079, NHpoly99th$rcp85_20502079, "days per year"),
                           polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipgraph1Proj.eps"),
                           polycaption = polyviocapDay)
colnames(locationADaypoly) <- cnames

locationBEventpoly = data.frame(location = CAlocation, obs=round(CAcurrent_total_precip,1),
                                dayobs = round(CAcurrent_day_precip), eventpoly = round(CApoly99thres$obsmean,1), 
                                daypoly = round(CApoly99th$obsmean,1), val = "1% event",
                                likelihood = double.likl(CApoly99thres$likl.increase45, CApoly99thres$likl.increase85, "intensify"),
                                futpoly = double.change(CApoly99thres$percentobsincrease45, CApoly99thres$percentobsincrease85,
                                                        CApoly99thres$rcp45_20502079, CApoly99thres$rcp85_20502079, "inches"),
                                polyfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99th-polygraph1.eps"),
                                polycaption = polyviocapEvent)
colnames(locationBEventpoly) <- cnames

locationBDaypoly = data.frame(location = CAlocation, obs=round(CAcurrent_total_precip,1),
                              dayobs = round(CAcurrent_day_precip), eventpoly = round(CApoly99thres$obsmean,1),
                              daypoly = round(CApoly99th$obsmean,1), val = "number of days with a 1% event", 
                              likelihood = double.likl(CApoly99th$likl.increase45, CApoly99th$likl.increase85, "increase"),
                              futpoly = double.change(CApoly99th$percentobsincrease45, CApoly99th$percentobsincrease, 
                                                      CApoly99th$rcp45_20502079, CApoly99th$rcp85_20502079, "days per year"),
                              polydaysfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99thdays-precipgraph1Proj.eps"),
                              polycaption = polyviocapDay)
colnames(locationBDaypoly) <- cnames

outlookspoly = rbind(locationAEventpoly, locationADaypoly, locationBEventpoly, locationBDaypoly)
encodeConnection <- file("outlooks/outlooks_merge_poly.txt",encoding = "UTF-16")
write.table(outlookspoly, file=encodeConnection, row.names=FALSE, sep = "\t")

# Bar graphs ----------------------------------------------------------
locationAbar = data.frame(location = NHlocation, obs=round(NHpolytot$hindcastbaseline,1),
                          dayobs = round(NHpoly0p5$hindcastbaseline), eventpoly = round(NHbar99thres$hindcastmean,1),
                          daypoly = round(NHbar99th$baseline,1), 
                          futpoly = paste(round(NHbar99thres$rcp45_20502079,1), "to", round(NHbar99thres$rcp85_20502079,1)),
                          futdayspoly = paste(round(NHbar99th$rcp45_20502079,1), "to", round(NHbar99th$rcp85_20502079,1)),
                          perpoly = paste(round(NHbar99thres$percenthindincrease45), "to", round(NHbar99thres$percenthindincrease85)),
                          perdayspoly = paste(round(NHbar99th$percenthindincrease45), "to", round(NHbar99th$percenthindincrease)),
                          polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-bargraph1.eps"),
                          polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipchangeBargraph1.eps"),
                          polycaption = barcap)
colnames(locationAbar) <- cnames

locationBbar = data.frame(location = CAlocation, obs=round(CApolytot$hindcastbaseline,1),
                           dayobs = round(CApoly0p5$hindcastbaseline), eventpoly = round(CAbar99thres$hindcastmean,1),
                           daypoly = round(CAbar99th$baseline,1), 
                           futpoly = paste(round(CAbar99thres$rcp45_20502079,1), "to", round(CAbar99thres$rcp85_20502079,1)),
                           futdayspoly = paste(round(CAbar99th$rcp45_20502079,1), "to", round(CAbar99th$rcp85_20502079,1)),
                           perpoly = paste(round(CAbar99thres$percenthindincrease45), "to", round(CAbar99thres$percenthindincrease85)),
                           perdayspoly = paste(round(CAbar99th$percenthindincrease45), "to", round(CAbar99th$percenthindincrease)),
                           polyfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99th-bargraph1.eps"),
                           polydaysfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99thdays-precipchangeBargraph1.eps"),
                           polycaption = barcap)
colnames(locationBbar) <- cnames

outlooksbar = rbind(locationAbar, locationBbar)
encodeConnection <- file("outlooks/outlooks_merge_bar.txt",encoding = "UTF-16")
write.table(outlooksbar, file=encodeConnection, row.names=FALSE, sep = "\t")

# Box graphs ----------------------------------------------------------
locationAbox = data.frame(location = NHlocation, obs=round(NHcurrent_total_precip,1),
                          dayobs = round(NHcurrent_day_precip), eventpoly = round(NHbox99thres$obsmean,1),
                          daypoly = round(NHbox99th$obsmean,1), 
                          futpoly = paste(round(NHbox99thres$rcp45_20502079,1), "to", round(NHbox99thres$rcp85_20502079,1)),
                          futdayspoly = paste(round(NHbox99th$rcp45_20502079,1), "to", round(NHbox99th$rcp85_20502079,1)),
                          perpoly = paste(round(NHbox99thres$percentobsincrease45), "to", round(NHbox99thres$percentobsincrease85)),
                          perdayspoly = paste(round(NHbox99th$percentobsincrease45), "to", round(NHbox99th$percentobsincrease)),
                          polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-boxgraph1.eps"),
                          polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipBoxgraph1.eps"),
                          polycaption = polyviocap)
colnames(locationAbox) <- cnames

locationBbox = data.frame(location = CAlocation, obs=round(CAcurrent_total_precip,1),
                          dayobs = round(CAcurrent_day_precip), eventpoly = round(CAbox99thres$obsmean,1),
                          daypoly = round(CAbox99th$obsmean,1), 
                          futpoly = paste(round(CAbox99thres$rcp45_20502079,1), "to", round(CAbox99thres$rcp85_20502079,1)),
                          futdayspoly = paste(round(CAbox99th$rcp45_20502079,1), "to", round(CAbox99th$rcp85_20502079,1)),
                          perpoly = paste(round(CAbox99thres$percentobsincrease45), "to", round(CAbox99thres$percentobsincrease85)),
                          perdayspoly = paste(round(CAbox99th$percentobsincrease45), "to", round(CAbox99th$percentobsincrease)),
                          polyfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99th-boxgraph1.eps"),
                          polydaysfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99thdaysprecipBoxgraph1.eps"),
                          polycaption = polyviocap)
colnames(locationBbox) <- cnames

outlooksbox = rbind(locationAbox, locationBbox)
encodeConnection <- file("outlooks/outlooks_merge_box.txt",encoding = "UTF-16")
write.table(outlooksbox, file=encodeConnection, row.names=FALSE, sep = "\t")
# 
# # Violin graphs ----------------------------------------------------------
# locationAvio = data.frame(location = NHlocation, obs=round(NHcurrent_total_precip,1),
#                           dayobs = round(NHcurrent_day_precip), eventpoly = round(NHviolin99thres$obsmean,1),
#                           daypoly = round(NHviolin99th$obsmean,1), 
#                           futpoly = paste(round(NHviolin99thres$rcp45_20502079,1), "to", round(NHviolin99thres$rcp85_20502079,1)),
#                           futdayspoly = paste(round(NHviolin99th$rcp45_20502079,1), "to", round(NHviolin99th$rcp85_20502079,1)),
#                           perpoly = paste(round(NHviolin99thres$percentobsincrease45), "to", round(NHviolin99thres$percentobsincrease85)),
#                           perdayspoly = paste(round(NHviolin99th$percentobsincrease45), "to", round(NHviolin99th$percentobsincrease)),
#                           polyfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99th-viograph1.eps"),
#                           polydaysfig=paste0(NHplotDir, sub("_.*", "", NHstabv),"-shp-99thdays-precipHistgraph1.eps"),
#                           polycaption = polyviocap)
# colnames(locationAvio) <- cnames
# 
# locationBvio = data.frame(location = CAlocation, obs=round(CAcurrent_total_precip,1),
#                            dayobs = round(CAcurrent_day_precip), eventpoly = round(CAviolin99thres$obsmean,1),
#                            daypoly = round(CAviolin99th$obsmean,1), 
#                            futpoly = paste(round(CAviolin99thres$rcp45_20502079,1), "to", round(CAviolin99thres$rcp85_20502079,1)),
#                            futdayspoly = paste(round(CAviolin99th$rcp45_20502079,1), "to", round(CAviolin99th$rcp85_20502079,1)),
#                            perpoly = paste(round(CAviolin99thres$percentobsincrease45), "to", round(CAviolin99thres$percentobsincrease85)),
#                            perdayspoly = paste(round(CAviolin99th$percentobsincrease45), "to", round(CAviolin99th$percentobsincrease)),
#                            polyfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99th-viograph1.eps"),
#                            polydaysfig=paste0(CAplotDir, sub("_.*", "", CAstabv),"-shp-99thdays-precipHistgraph1.eps"),
#                            polycaption = polyviocap)
# colnames(locationBvio) <- cnames
# 
# outlooksvio = rbind(locationAvio, locationBvio)
# encodeConnection <- file("outlooks/outlooks_merge_vio.txt",encoding = "UTF-16")
# write.table(outlooksvio, file=encodeConnection, row.names=FALSE, sep = "\t")

##########################################################################
# END
##########################################################################

increase = rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")] - obsmean
plot(density(increase))

# What is the percent with values above zero?
(length(which(increase > 0))/length(increase))*100

