##########################################################################
##########################################################################
## Script Name: precip_vis.R
## Purpose of Script: This script creates different graphs of precipitation
## using the same data. The precip graphs are based on the extracted and 
## summarized MACA and GridMet data (done on ROAR). 
##
## Author: Kelsey Ruckert
## Email: klr324@psu.edu
## Date: 11/2/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################

# library(plyr)
# library(dplyr)
library(zoo)
library(Cairo)
# library(plotly)
# library(reshape2)
# library(ggplot2)
# library(ggthemes)
#library(export)
library(measurements)
library(stringr)
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
# What state?
stabv = "VA"

dataDir <- "data/"
precipDir <- "plots/"

# Generate precipDir directory if it doesn't already exist
# script will error out otherwise
if(!dir.exists(precipDir)){
  dir.create(precipDir, recursive=TRUE)
}

##########################################################################
# Read in precipitation data files
##########################################################################
wet1inAve <- read.csv(paste0(dataDir, "vaCountyMACA_1in2.0_full.csv"))
wet1indays <- read.csv(paste0(dataDir, "vaCountyMACA_1inByYr2.0_full.csv"))
wet1inObs <- read.csv(paste0(dataDir, "va1incountyGridMet_wetDaysByYr.csv"))

wet2inAve <- read.csv(paste0(dataDir, "vaCountyMACA_2in2.0_full.csv"))
wet2indays <- read.csv(paste0(dataDir, "vaCountyMACA_2inByYr2.0_full.csv"))
wet2inObs <- read.csv(paste0(dataDir, "va2incountyGridMet_wetDaysByYr.csv"))

wetseasdays <- read.csv(paste0(dataDir, "vaCountyMACA_seas2inByYr2.0_full.csv"))
wetseasObs <- read.csv(paste0(dataDir, "vaseas2incountyGridMet_wetDaysByYr.csv"))
winter2inAve <- read.csv(paste0(dataDir, "vaCountyMACA_2in_winter2.0_full.csv"))
spring2inAve <- read.csv(paste0(dataDir, "vaCountyMACA_2in_spring2.0_full.csv"))
summer2inAve <- read.csv(paste0(dataDir, "vaCountyMACA_2in_summer2.0_full.csv"))
fall2inAve <- read.csv(paste0(dataDir, "vaCountyMACA_2in_fall2.0_full.csv"))

cycle2in1950 <- read.csv(paste0(dataDir, "vaCountyMACA_2in_1950_full.csv"))
cycle2in2020 <- read.csv(paste0(dataDir, "vaCountyMACA_2in_2020_full.csv"))

cycletot1950 <- read.csv(paste0(dataDir, "vaCountyMACA_tot_1950_full.csv"))
cycletot2020 <- read.csv(paste0(dataDir, "vaCountyMACA_tot_2020_full.csv"))

changeFactor <- read.csv(paste0(dataDir, "vaCountyMACA_changeFactor_full.csv"))
idfPulaski <- read.csv(paste0(dataDir, "UCL_Depth_English_PDS.csv"), skip=13)

wettotObs <- read.csv(paste0(dataDir, "vacountyGridMet_totByYr.csv"))
totseasObs <- read.csv(paste0(dataDir, "vacountyGridMet_seastotByYr.csv"))

# Pulaski county
fips = 51155
##########################################################################
# Current information
##########################################################################

total.obs <- wettotObs[wettotObs$FIPS==fips,]
current_total_precip <- mean(total.obs$wetDays[which(total.obs$year %in% 1990:2019)])/25.4
print(paste("Average annual precipitation is", current_total_precip, "inches"))

ext2.obs <- wet2inObs[wet2inObs$FIPS==fips,]
current_2in_precip <- mean(ext2.obs$wetDays[which(ext2.obs$year %in% 1990:2019)])
print(paste("Average number of days per year ≥ 2 inches is", current_2in_precip))

season.total.obs <- totseasObs[totseasObs$FIPS==fips,]
winter_precip <- mean(season.total.obs$winter[which(ext2.obs$year %in% 1990:2019)], na.rm=TRUE)/25.4
spring_precip <- mean(season.total.obs$spring[which(ext2.obs$year %in% 1990:2019)], na.rm=TRUE)/25.4
summer_precip <- mean(season.total.obs$summer[which(ext2.obs$year %in% 1990:2019)], na.rm=TRUE)/25.4
fall_precip <- mean(season.total.obs$fall[which(ext2.obs$year %in% 1990:2019)], na.rm=TRUE)/25.4
cat(paste("Average winter precipitation is", winter_precip, "inches\n", 
            "Average spring precipitation is", spring_precip, "inches\n", 
            "Average summer precipitation is", summer_precip, "inches\n", 
            "Average fall precipitation is", fall_precip, "inches"))

##########################################################################
# Create figures
##########################################################################
##########################################################################
# Precipitation - as Polygon graph
##########################################################################
poly2in = calcPrecipRunAveGraph(fips,
                   dataAveTab = wet2inAve, dataTab=wet2indays,
                   obsTab = wet2inObs,
                    ylabel = "Number of days with\nprecipitation ≥ 2in",
                    var="2in",
                    outDir=precipDir,
                    create.plot=create.plot)

##########################################################################
# Precipitation Change Bar Graph
##########################################################################
bar2in = calcPrecipChangeBarGraph(fips, dataTab=wet2indays,  
                    ylabel = "Percent change in days with\nprecipitation ≥ 2in", 
                    var="2in",outDir=precipDir, create.plot=create.plot)

##########################################################################
# Precipitation Violin Graph
##########################################################################
violin2in = calcPrecipViolin(fips, dataTab=wet2indays, obsTab = wet2inObs,
                             ylabel = "Number of days with\nprecipitation ≥ 2in",
                             var="2in", percent=FALSE, outDir=precipDir,
                             create.plot=TRUE, leg.inside = FALSE)

##########################################################################
# IDF Graph
##########################################################################
calcIDF(fips, changeFactor, idf=idfPulaski, 
        rp=c(2, 5, 10, 25, 50, 100), outDir=precipDir)

##########################################################################
##########################################################################
# SEASONAL GRAPHS
##########################################################################
##########################################################################
# Seasonal Cycle Total Precip. Graph
##########################################################################
calcSeasCycle(fips, pastdat = cycletot1950, futdat = cycletot2020, 
              type="tot", outDir=precipDir, ylabel = "Total precipitation (in)")

calcSeasCycle(fips, pastdat = cycle2in1950, futdat = cycle2in2020, 
              type="2in", outDir=precipDir, ylabel = "Number of days with precipitation ≥ 2in")

##########################################################################
# Seasonal Precipitation Violin Graph
##########################################################################
calcSeasPrecipViolin(fips, dataTab=wetseasdays, obsTab = wetseasObs,
                 ylabel = "Number of days with precipitation ≥ 2in",
                 var="2in", percent=FALSE, outDir=precipDir, create.plot=TRUE,
                 leg.inside = FALSE, add.legend = FALSE)

##########################################################################
# Seasonal Precipitation Change Bar Graph
##########################################################################
calcSeasPrecipChangeBarGraph(fips, dataTab=wetseasdays, 
                             ylabel = "Percent change in days with precipitation ≥ 2in", 
                             var="2in", outDir=precipDir, create.plot=create.plot)

##########################################################################
# Seasonal Precipitation - as Polygon graph
##########################################################################
cairo_ps(paste0(precipDir, unique(wettotObs[wettotObs$FIPS==fips,]$county), 
                "seasonprecipgraph1Proj.eps"), width=8, height=6)

par(mgp=c(1.5,.5,0), mfrow=c(2,2), las=1)
# winter ------------------------------------------------------------------
par(mgp=c(1.5,.5,0), mar=c(0.5, 4, 2.5, 0), las=1)
winter2in = calcSeasPrecipRunAveGraph(fips, 
                   dataAveTab = winter2inAve, dataTab=wetseasdays,
                   obsTab = wetseasObs,
                    ylabel = "",
                    var="2in",
                   season="winter",
                    outDir=paste0(precipDir, "winter2in/"),
                    create.plot=create.plot, xaxis = FALSE)

legend("topleft", "Winter", bty="n")

mtext(side=2, line=-1.5, "Number of days with precipitation ≥ 2in", font=1, las=0, outer=TRUE)
# spring ------------------------------------------------------------------
par(mgp=c(1.5,.5,0), mar=c(0.5, 1.5, 2.5, 1), las=1)
spring2in = calcSeasPrecipRunAveGraph(fips,
                   dataAveTab = spring2inAve, dataTab=wetseasdays,
                   obsTab = wetseasObs,
                   ylabel = "",
                   var="2in",
                   season="spring",
                   outDir=paste0(precipDir, "spring2in/"),
                   create.plot=create.plot, xaxis = FALSE)

legend("topleft", "Spring", bty="n")
# summer ------------------------------------------------------------------
par(mgp=c(1.5,.5,0), mar=c(2.5, 4, 0, 0), las=1)
summer2in = calcSeasPrecipRunAveGraph(fips,
                   dataAveTab = summer2inAve, dataTab=wetseasdays,
                   obsTab = wetseasObs,
                   ylabel = "",
                   var="2in",
                   season="summer",
                   outDir=paste0(precipDir, "summer2in/"),
                   create.plot=create.plot)
legend("topleft", "Summer", bty="n")

# fall --------------------------------------------------------------------
par(mgp=c(1.5,.5,0), mar=c(2.5, 1.5, 0, 1), las=1)
fall2in = calcSeasPrecipRunAveGraph(fips,
                   dataAveTab = fall2inAve, dataTab=wetseasdays,
                   obsTab = wetseasObs,
                   ylabel = "",
                   var="2in",
                   season="fall",
                   outDir=paste0(precipDir, "fall2in/"),
                   create.plot=create.plot)
legend("topleft", "Fall", bty="n")

# Constructing Legend -----------------------------------------------------
colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
            rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")

# Transparent colors with transparent color function
trans_colors100 = makeTransparent(colors, 100)
trans_colors150 = makeTransparent(colors, 150)
trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])

scenarios <- c("Observations", "Hindcast Range", "Low Emissions Average",
               "High Emissions Average", "Low Emissions Range",
               "High Emissions Range")

add_legend("topright", legend = scenarios,
           pch = c(20, 15, NA, NA, 15, 15), lty=c(NA, NA, 1, 1, NA, NA),
           lwd=c(NA, NA, 2, 2, NA, NA), pt.cex = c(1, 2, NA, NA, 2, 2),
           col = c(colors[6], trans_colors[5], colors[1:2], trans_colors[3:4]),
           bty='n', cex=1, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))#c(0.47, 0.47, 0.43, 0.43, 0.35, 0.35))

dev.off()

##########################################################################
# END
##########################################################################



