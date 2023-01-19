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
stabv = "wascoCA_150km"
location = "Location B"
plotDir = paste0("Users:klr324:Documents:Github:precipVisualization:plots:", stabv, ":")

gridDir <- paste0("TimeSeries_GridMet/", stabv, "/")
macaDir <- paste0("TimeSeries_MACA/", stabv, "/")
# imageDir <- "~/Documents/Github/communityClimateOutlooks/creatingCountyOutlooks/FormattingComponents/"
precipDir <- paste0("plots/", stabv, "/")

# Generate precipDir directory if it doesn't already exist
# script will error out otherwise
if(!dir.exists(precipDir)){
  dir.create(precipDir, recursive=TRUE)
}

##########################################################################
# Read in precipitation data files
##########################################################################
# 1 in --------------------------------------------------------------------
wet1inAve <- read.csv(paste0(macaDir, stabv, "CountyMACA_1inwetDays_full.csv"))
wet1indays <- read.csv(paste0(macaDir, stabv, "CountyMACA_1inwetDaysByYr_full.csv"))
wet1inObs <- read.csv(paste0(gridDir, stabv, "1incountyGridMet_wetDaysByYr.csv"))

# 2 in --------------------------------------------------------------------
wet2inAve <- read.csv(paste0(macaDir, stabv, "CountyMACA_2inwetDays_full.csv"))
wet2indays <- read.csv(paste0(macaDir, stabv, "CountyMACA_2inwetDaysByYr_full.csv"))
wet2inObs <- read.csv(paste0(gridDir, stabv, "2incountyGridMet_wetDaysByYr.csv"))

# Annual total ------------------------------------------------------------
wettotAve <- read.csv(paste0(macaDir, stabv, "CountyMACA_tot_full.csv"))
wettotdays <- read.csv(paste0(macaDir, stabv, "CountyMACA_totByYr_full.csv"))
wettotObs <- read.csv(paste0(gridDir, stabv, "countyGridMet_totByYr.csv"))

# 99th Annual total ------------------------------------------------------------
wet99totAve <- read.csv(paste0(macaDir, stabv, "CountyMACA_99thtot_full.csv"))
wet99totdays <- read.csv(paste0(macaDir, stabv, "CountyMACA_99thtotByYr_full.csv"))
wet99totObs <- read.csv(paste0(gridDir, stabv, "countyGridMet_99thtotByYr.csv"))
wet99totObsAve <- read.csv(paste0(gridDir, stabv, "countyGridMet_99thtot.csv"))
# 
# # 2in season cycle --------------------------------------------------------
# cycle2in1950 <- read.csv(paste0(macaDir, stabv, "CountyMACA_2in_cent1950_full.csv"))
# cycle2in2020 <- read.csv(paste0(macaDir, stabv, "CountyMACA_2in_cent2020_full.csv"))
# cycle2in2050 <- read.csv(paste0(macaDir, stabv, "CountyMACA_2in_fut2050_full.csv"))
# 
# # total season cycle --------------------------------------------------------
# cycletot1950 <- read.csv(paste0(macaDir, stabv, "CountyMACA_tot_cent1950_full.csv"))
# cycletot2020 <- read.csv(paste0(macaDir, stabv, "CountyMACA_tot_cent2020_full.csv"))
# cycletot2050 <- read.csv(paste0(macaDir, stabv, "CountyMACA_tot_fut2050_full.csv"))
# 
# # IDF curve ---------------------------------------------------------------
# changeFactor2020 <- read.csv(paste0(macaDir, stabv, "CountyMACA_changeFactor_cent_full.csv"))
# changeFactor2050 <- read.csv(paste0(macaDir, stabv, "CountyMACA_changeFactor_fut_full.csv"))
# idfAtlas14 <- read.csv("data/PF_Depth_English_PDS_wascoCA.csv", skip=13)

##########################################################################
# Current information
##########################################################################

current_total_precip <- mean(wettotObs$wetDays[which(wettotObs$year %in% 1990:2019)])/25.4
print(paste("Average annual precipitation is", current_total_precip, "inches"))

current_2in_precip <- mean(wet2inObs$wetDays[which(wet2inObs$year %in% 1990:2019)])
print(paste("Average number of days per year ≥ 2 inches is", current_2in_precip))

current_1in_precip <- mean(wet1inObs$wetDays[which(wet1inObs$year %in% 1990:2019)])
print(paste("Average number of days per year ≥ 1 inches is", current_1in_precip))

current_99thtotal_precip <- mean(wet99totObs$wetDays[which(wet99totObs$year %in% 1990:2019)])/25.4
print(paste("The 1% event precipitation is", (wet99totObsAve$threshold[1])/25.4, "inches (1990-2019)"))
print(paste("Average annual total 1% event precipitation is", current_99thtotal_precip, "inches"))

##########################################################################
# Create figures
##########################################################################

# 2 inches ----------------------------------------------------------------
##########################################################################
# Precipitation - as Polygon graph
##########################################################################
poly2in = calcPrecipRunAveGraph("shp", dataAveTab = wet2inAve, dataTab=wet2indays,
                   obsTab = wet2inObs, ylabel = "Number of days with\nprecipitation at least 2in",
                    var="2in", outDir=precipDir, create.plot=create.plot)

poly1in = calcPrecipRunAveGraph("shp", dataAveTab = wet1inAve, dataTab=wet1indays,
                                obsTab = wet1inObs, ylabel = "Number of days with\nprecipitation at least 1in",
                                var="1in", outDir=precipDir, create.plot=create.plot)

polytot = calcPrecipRunAveGraph("shp", dataAveTab = wettotAve, dataTab=wettotdays,
                                obsTab = wettotObs, ylabel = "Annual total precipitation (in)",
                                var="tot", outDir=precipDir, create.plot=create.plot)

poly99tot = calcPrecipRunAveGraph("shp", dataAveTab = wet99totAve, dataTab=wet99totdays,
                                obsTab = wet99totObs, ylabel = "Annual total precipitation from\nheaviest 1% events (in)",
                                var="99tot", outDir=precipDir, create.plot=create.plot)

##########################################################################
# Precipitation Change Bar Graph
##########################################################################
bar2in = calcPrecipChangeBarGraph("shp", dataTab=wet2indays,  
                    ylabel = "Percent change in days with\nprecipitation at least 2in", 
                    var="2in",outDir=precipDir, create.plot=create.plot)

bar1in = calcPrecipChangeBarGraph("shp", dataTab=wet1indays,  
                                  ylabel = "Percent change in days with\nprecipitation at least 1in", 
                                  var="1in",outDir=precipDir, create.plot=create.plot)

bartot = calcPrecipChangeBarGraph("shp", dataTab=wettotdays,  
                                  ylabel = "Percent change in annual total precipitation", 
                                  var="tot",outDir=precipDir, create.plot=create.plot)

bar99tot = calcPrecipChangeBarGraph("shp", dataTab=wet99totdays,  
                                  ylabel = "Percent change in annual total precipitation\nfrom heaviest 1% events", 
                                  var="99tot",outDir=precipDir, create.plot=create.plot)

##########################################################################
# Precipitation Violin Graph
##########################################################################
violin2in = calcPrecipViolin("shp", dataTab=wet2indays, obsTab = wet2inObs,
                             ylabel = "Number of days with\nprecipitation at least 2in",
                             var="2in", percent=FALSE, outDir=precipDir,
                             create.plot=TRUE, leg.inside = FALSE)

violin1in = calcPrecipViolin("shp", dataTab=wet1indays, obsTab = wet1inObs,
                             ylabel = "Number of days with\nprecipitation at least 1in",
                             var="1in", percent=FALSE, outDir=precipDir,
                             create.plot=TRUE, leg.inside = FALSE)

violintot = calcPrecipViolin("shp", dataTab=wettotdays, obsTab = wettotObs,
                             ylabel = "Annual total precipitation (in)",
                             var="tot", percent=FALSE, outDir=precipDir,
                             create.plot=TRUE, leg.inside = FALSE)

violin99tot = calcPrecipViolin("shp", dataTab=wet99totdays, obsTab = wet99totObs,
                             ylabel = "Annual total precipitation\nfrom heaviest 1% events (in)",
                             var="99tot", percent=FALSE, outDir=precipDir,
                             create.plot=TRUE, leg.inside = FALSE)

##########################################################################
# IDF Graph
##########################################################################
# calcIDF(fips, changeFactor, idf=idfPulaski,
#         rp=c(2, 5, 10, 25, 50, 100), outDir=precipDir)
# 
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=2, timeframe="2020-2069", outDir=precipDir)
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=5, timeframe="2020-2069", outDir=precipDir)
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=10, timeframe="2020-2069", outDir=precipDir)
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=25, timeframe="2020-2069", outDir=precipDir)
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=50, timeframe="2020-2069", outDir=precipDir)
# calcIDF_range("shp", changeFactor2020, idf=idfAtlas14, rp=100, timeframe="2020-2069", outDir=precipDir)
# 
# calcIDF_depth("shp", changeFactor2020, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               2, timeframe="2020-2069",outDir=precipDir)
# calcIDF_depth("shp", changeFactor2020, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               1, timeframe="2020-2069",outDir=precipDir)
# calcIDF_depth("shp", changeFactor2020, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               (wet99totObsAve$threshold[1])/25.4, timeframe="2020-2069", outDir=precipDir)
# 
# # 2050-2099 ---------------------------------------------------------------
# # calcIDF(fips, changeFactor2050, idf=idfPulaski, 
# #         rp=c(2, 5, 10, 25, 50, 100), outDir=precipDir2050)
# 
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=2, timeframe="2050-2099", outDir=precipDir)
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=5, timeframe="2050-2099", outDir=precipDir)
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=10, timeframe="2050-2099", outDir=precipDir)
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=25, timeframe="2050-2099", outDir=precipDir)
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=50, timeframe="2050-2099", outDir=precipDir)
# calcIDF_range("shp", changeFactor2050, idf=idfAtlas14, rp=100, timeframe="2050-2099", outDir=precipDir)
# 
# calcIDF_depth("shp", changeFactor2050, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               2, timeframe="2050-2099", outDir=precipDir)
# calcIDF_depth("shp", changeFactor2050, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               1, timeframe="2050-2099", outDir=precipDir)
# calcIDF_depth("shp", changeFactor2050, idf=idfAtlas14, rp=c(2, 5, 10, 25, 50, 100), 
#               (wet99totObsAve$threshold[1])/25.4, timeframe="2050-2099", outDir=precipDir)
# 
# ##########################################################################
# # Seasonal Cycle Total Precip. Graph
# ##########################################################################
# calcSeasCycle("shp", pastdat = cycle2in1950, futdat = cycle2in2020, 
#               type="2in", outDir=precipDir, ylabel = "Number of days with precipitation at least 2in")
# 
# calcSeasCycle("shp", pastdat = cycletot1950, futdat = cycletot2020, 
#               type="tot", outDir=precipDir, ylabel = "Total precipitation (in)")
# 
# # 2050 - 2099 -------------------------------------------------------------
# calcSeasCycle("shp", pastdat = cycle2in1950, futdat = cycle2in2050, futyear=2050,
#               type="2in", outDir=precipDir, ylabel = "Number of days with precipitation at least 2in")
# 
# calcSeasCycle("shp", pastdat = cycletot1950, futdat = cycletot2050, futyear=2050, 
#               type="tot", outDir=precipDir, ylabel = "Total precipitation (in)")

##########################################################################
# Outlooks
##########################################################################
cnames = c("location", "annual-rainfall", "heavy-event-info", "variable", 
           "percent-increase-poly", "percent-increase-bar", "percent-increase-violin",
           "@polyfig", "@barfig", "@viofig", "polycaption", "barcaption", "viocaption",
           "future-average-poly", "future-average-bar", "future-average-vio")

barcap = function(variable){paste("The graph shows the change in", variable, 
                                  "compared to the average between 1990 and 2019. The gray bars show the hindcast,",
                                  "(historic model results). Two scenarios of the future are shown",
                                  "as a high emissions scenario (RCP 8.5) in red and a low-emissions scenario", 
                                  "(RCP 4.5) in blue. Data retrieved from Multivariate Adaptive Constructed Analogs (MACA).")}

polycap = function(variable){paste0("The graph shows ", variable, ". Gray dots represent observations and",
                                    " the gray shading shows the hindcast (historic model results).",
                                    " Two scenarios of the future are shown as a high emissions scenario (RCP 8.5) in red",
                                    " and a low-emissions scenario (RCP 4.5) in blue. Data",
                                    " retrieved from Multivariate Adaptive Constructed Analogs (MACA) and",
                                    " Gridded Surface Meteorological Dataset (gridMET).")}

viocap = function(variable){paste0("The graph shows ", variable, ". Gray dots represent observations and",
                                   " the gray shading shows the hindcast (historic model results).",
                                   " Two scenarios of the future are shown as a high emissions scenario (RCP 8.5) in red",
                                   " and a low-emissions scenario (RCP 4.5) in blue. Data",
                                   " retrieved from Multivariate Adaptive Constructed Analogs (MACA) and",
                                   " Gridded Surface Meteorological Dataset (gridMET).")}

# Annual total precipitation from heaviest 1% of events -------------------
var99th_df = data.frame(location = location, annual=round(current_total_precip,1), 
                        info=paste("year (1990-2019 average) with", round(current_99thtotal_precip,1), 
                                   "inches coming from heavy rainfall events like the 1% event.", 
                                   "A 1% event occurs when rainfall exceeds", round((wet99totObsAve$threshold[1])/25.4,1), 
                                   "inches in a 24-hour period."), 
                        variable="The annual total rain from 1% rainfall events", 
                        poly=paste(round(poly99tot$percenttrendincrease45), "to", 
                                   round(poly99tot$percenttrendincrease85)),
                        bar=paste(round(bar99tot$increase45), "to", round(bar99tot$increase85)),
                        vio=paste(round(violin99tot$percenttrendincrease45), "to", 
                                  round(violin99tot$percenttrendincrease85)),
                        polyfig=paste0(plotDir, sub("_.*", "", stabv), 
                                       "-shp-99tot-precipgraph1Proj.eps"),
                        barfig=paste0(plotDir, sub("_.*", "", stabv), 
                                      "-shp-99tot-precipchangeBargraph1.eps"),
                        viofig=paste0(plotDir, sub("_.*", "", stabv), 
                                      "-shp-99tot-precipHistgraph1.eps"),
                        polycaption = polycap("the annual total rain from 1% rainfall events"),
                        barcaption = barcap("the annual total rain from 1% rainfall events"),
                        viocaption = viocap("the annual total rain from 1% rainfall events"),
                        futureavepoly = paste(round(poly99tot$rcp45_20502079, 1), "to", 
                                          round(poly99tot$rcp85_20502079, 1), "inches"),
                        futureavebar = paste(round((bar99tot$rcp45_20502079/25.4), 1), "to", 
                                              round((bar99tot$rcp85_20502079/25.4), 1), "inches"),
                        futureavevio = paste(round(violin99tot$rcp45_20502079, 1), "to", 
                                              round(violin99tot$rcp85_20502079, 1), "inches"))
colnames(var99th_df) <- cnames

# 1 inches ----------------------------------------------------------------
var1in_df = data.frame(location = location, annual=round(current_total_precip,1), 
                       info=paste("year (1990-2019 average) with heavy rainfall events of at least 1 inch occurring an average of", 
                                  round(current_1in_precip,1), "days a year."), 
                       variable="The number of days per year with at least 1 inch of rainfall", 
                       poly=paste(round(poly1in$percenttrendincrease45), "to", 
                                  round(poly1in$percenttrendincrease85)),
                       bar=paste(round(bar1in$increase45), "to", round(bar1in$increase85)),
                       vio=paste(round(violin1in$percenttrendincrease45), "to", 
                                 round(violin1in$percenttrendincrease85)),
                       polyfig=paste0(plotDir, sub("_.*", "", stabv), 
                                      "-shp-1in-precipgraph1Proj.eps"),
                       barfig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-1in-precipchangeBargraph1.eps"),
                       viofig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-1in-precipHistgraph1.eps"),
                       polycaption = polycap("the number of days per year with at least 1 inch of rainfall"),
                       barcaption = barcap("the number of days per year with at least 1 inch of rainfall"),
                       viocaption = viocap("the number of days per year with at least 1 inch of rainfall"),
                       futureavepoly = paste(round(poly1in$rcp45_20502079, 1), "to", 
                                             round(poly1in$rcp85_20502079, 1), "days"),
                       futureavebar = paste(round(bar1in$rcp45_20502079, 1), "to", 
                                            round(bar1in$rcp85_20502079, 1), "days"),
                       futureavevio = paste(round(violin1in$rcp45_20502079, 1), "to", 
                                            round(violin1in$rcp85_20502079, 1), "days"))
colnames(var1in_df) <- cnames

# 2 inches ----------------------------------------------------------------
var2in_df = data.frame(location = location, annual=round(current_total_precip,1), 
                       info=paste("year (1990-2019 average) with heavy rainfall events of at least 2 inches occurring an average of", 
                                  round(current_2in_precip,1), "days a year."), 
                       variable="The number of days per year with at least 2 inch of rainfall", 
                       poly=paste(round(poly2in$percenttrendincrease45), "to", 
                                  round(poly2in$percenttrendincrease85)),
                       bar=paste(round(bar2in$increase45), "to", round(bar2in$increase85)),
                       vio=paste(round(violin2in$percenttrendincrease45), "to", 
                                 round(violin2in$percenttrendincrease85)),
                       polyfig=paste0(plotDir, sub("_.*", "", stabv), 
                                      "-shp-2in-precipgraph1Proj.eps"),
                       barfig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-2in-precipchangeBargraph1.eps"),
                       viofig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-2in-precipHistgraph1.eps"),
                       polycaption = polycap("the number of days per year with at least 2 inches of rainfall"),
                       barcaption = barcap("the number of days per year with at least 2 inches of rainfall"),
                       viocaption = viocap("the number of days per year with at least 2 inches of rainfall"),
                       futureavepoly = paste(round(poly2in$rcp45_20502079, 1), "to", 
                                             round(poly2in$rcp85_20502079, 1), "days"),
                       futureavebar = paste(round(bar2in$rcp45_20502079, 1), "to", 
                                            round(bar2in$rcp85_20502079, 1), "days"),
                       futureavevio = paste(round(violin2in$rcp45_20502079, 1), "to", 
                                            round(violin2in$rcp85_20502079, 1), "days"))
colnames(var2in_df) <- cnames


# Total precip ------------------------------------------------------------
varTot_df = data.frame(location = location, annual=round(current_total_precip,1), 
                       info="year (1990-2019 average).", 
                       variable="Annual total precipitation", 
                       poly=paste(round(polytot$percenttrendincrease45), "to", 
                                  round(polytot$percenttrendincrease85)),
                       bar=paste(round(bartot$increase45), "to", round(bartot$increase85)),
                       vio=paste(round(violintot$percenttrendincrease45), "to", 
                                 round(violintot$percenttrendincrease85)),
                       polyfig=paste0(plotDir, sub("_.*", "", stabv), 
                                      "-shp-tot-precipgraph1Proj.eps"),
                       barfig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-tot-precipchangeBargraph1.eps"),
                       viofig=paste0(plotDir, sub("_.*", "", stabv), 
                                     "-shp-tot-precipHistgraph1.eps"),
                       polycaption = polycap("annual total precipitation"),
                       barcaption = barcap("annual total precipitation"),
                       viocaption = viocap("annual total precipitation"),
                       futureavepoly = paste(round(polytot$rcp45_20502079, 1), "to", 
                                             round(polytot$rcp85_20502079, 1), "inches"),
                       futureavebar = paste(round((bartot$rcp45_20502079/25.4), 1), "to", 
                                            round((bartot$rcp85_20502079/25.4), 1), "inches"),
                       futureavevio = paste(round(violintot$rcp45_20502079, 1), "to", 
                                            round(violintot$rcp85_20502079, 1), "inches"))
colnames(varTot_df) <- cnames


outlooks = rbind(varTot_df, var99th_df, var1in_df, var2in_df)
encodeConnection <- file(paste0(precipDir, stabv, "_outlooks_merge.txt"), 
                         encoding = "UTF-16")
write.table(outlooks, file=encodeConnection, row.names=FALSE, sep = "\t")

##########################################################################
# END
##########################################################################

