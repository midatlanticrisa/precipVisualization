##########################################################################
# Plotting functions
##########################################################################
# Transparent Color Function 
makeTransparent<- function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor,2 ,
        function(curcoldata)
        {rgb(red=curcoldata[1],
             green=curcoldata[2],
             blue=curcoldata[3], alpha=alpha,
             maxColorValue=255)})
}

# -------------------------------------------------------------------------
# Plot a legend outside of the plotting area
# https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics/3932558
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

##########################################################################
# Precipitation Violin Graph
##########################################################################
calcPrecipViolin <- function(fips, dataTab, dataAveTab=NULL, obsTab, outDir, var, 
                             percent = TRUE,  season = NULL, ylabel, 
                             create.plot=TRUE, leg.inside = TRUE, smoothing=FALSE, 
                             add.legend = TRUE){
  #################
  # fips <- wet2indays$FIPS[1]
  # dataTab <- wet2indays
  # obsTab = wet2inObs
  # ylabel = "Number of days with\nprecipitation ≥ 2in"
  # percent=FALSE
  # outDir=precipDir
  # season = NULL
  # var = "2in"
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  if(!is.null(season)){
    if(season == "winter"){
      obsTab$wetDays = obsTab$winter
      dataTab$wetDays = dataTab$winter
      
      # Remove the first winter
      obsTab = obsTab[-which(obsTab$year %in% 1979), ]
      dataTab = dataTab[-which(dataTab$year %in% 1958), ]
      
    } else if(season == "winterspring"){
      obsTab$wetDays = obsTab$winter + obsTab$spring
      dataTab$wetDays = dataTab$winter + dataTab$spring
      
      # Remove the first winter
      obsTab = obsTab[-which(obsTab$year %in% 1979), ]
      dataTab = dataTab[-which(dataTab$year %in% 1958), ]
      
    } else if(season == "summerfall"){
      obsTab$wetDays = obsTab$summer + obsTab$fall
      dataTab$wetDays = dataTab$summer + dataTab$fall
      
    } else if(season == "spring"){
      obsTab$wetDays = obsTab$spring
      dataTab$wetDays = dataTab$spring
      
    } else if(season == "summer"){
      obsTab$wetDays = obsTab$summer
      dataTab$wetDays = dataTab$summer
      
    } else if(season == "fall"){
      obsTab$wetDays = obsTab$fall
      dataTab$wetDays = dataTab$fall
      
    } else{
      print("Season is not winter, spring, summer, or fall")
    }
  }
  
  # Set average periods -----------------------------------------------------
  tar.decades <- c(1960, 1970, 1980) ###CODE TO SET CHRONOLOGICAL BASELINE
  past.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  mid.decades <- c(2020, 2030, 2040) ###CODE TO SET CHRONOLOGICAL BASELINE
  future.decades <- c(2050, 2060, 2070)
  
  # Format observations -----------------------------------------------------
  county.obs <- obsTab[obsTab$FIPS==fips,]
  county.obs$decade <- round_any(county.obs$year, 10, floor)
  
  county.obs$period <- county.obs$decade 
  county.obs$period[which(county.obs$decade %in% tar.decades)] <- "1960-1989"
  county.obs$period[which(county.obs$decade %in% past.decades)] <- "1990-2019"
  county.obs$period[which(county.obs$decade %in% mid.decades)] <- "2020-2049"
  
  if(var == "tot"){
    county.obs$inches <- county.obs$wetDays/25.4
  } else {
    county.obs$inches <- county.obs$wetDays
  }
  
  # Format hindcasts and projections ----------------------------------------
  ##Loop to class each observation by decade, then appending as an extra column
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  decades <- unique(dataTab$decade)
  years <- unique(dataTab$year)
  
  # ##Calculations to determine baseline precip levels from 1950-1980
  # baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  ##Calculations to determine baseline precip levels from 1950-1980
  baseSelect <- dataTab[which((dataTab$decade %in% past.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- mean(baseSelect$wetDays) #/ nrow(baseSelect) # Includes rcp45 and rcp85 but values should be the same
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  
  # Remove partial 30-yr periods (1950s and 2080s)
  rcp45Tab = rcp45Tab[-which(rcp45Tab$decade %in% 1950), ]
  rcp45Tab = rcp45Tab[-which(rcp45Tab$decade %in% 2080), ]
  rcp85Tab = rcp85Tab[-which(rcp85Tab$decade %in% 1950), ]
  rcp85Tab = rcp85Tab[-which(rcp85Tab$decade %in% 2080), ]
  
  rcp45Tab$period <- rcp45Tab$decade 
  rcp45Tab$period[which(rcp45Tab$decade %in% tar.decades)] <- "1960-1989"
  rcp45Tab$period[which(rcp45Tab$decade %in% past.decades)] <- "1990-2019"
  rcp45Tab$period[which(rcp45Tab$decade %in% mid.decades)] <- "2020-2049"
  rcp45Tab$period[which(rcp45Tab$decade %in% future.decades)] <- "2050-2079"
  
  rcp85Tab$period <- rcp85Tab$decade 
  rcp85Tab$period[which(rcp85Tab$decade %in% tar.decades)] <- "1960-1989"
  rcp85Tab$period[which(rcp85Tab$decade %in% past.decades)] <- "1990-2019"
  rcp85Tab$period[which(rcp85Tab$decade %in% mid.decades)] <- "2020-2049"
  rcp85Tab$period[which(rcp85Tab$decade %in% future.decades)] <- "2050-2079"
  
  if(var == "tot"){
    rcp45Tab$inches <- rcp45Tab$wetDays/25.4
    rcp85Tab$inches <- rcp85Tab$wetDays/25.4
  } else {
    rcp45Tab$inches <- rcp45Tab$wetDays
    rcp85Tab$inches <- rcp85Tab$wetDays
  }
  
  county.rcp45.avg <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  dec = decades[-which(decades %in% c(1950, 2080))]
  
  rcp45Means <- sapply(dec, function(dec_yr){mean(rcp45Tab$inches[which(rcp45Tab$decade %in% dec_yr)])})
  rcp45df <- data.frame(means = rcp45Means, baseline = dec)
  
  rcp85Means <- sapply(dec, function(dec_yr){mean(rcp85Tab$inches[which(rcp85Tab$decade %in% dec_yr)])})
  rcp85df <- data.frame(means = rcp85Means, baseline = dec)
  
  # Export values ----------------------------------------------------------- 
  # round to nearest inch
  if(var == "tot"){
    baseline <- baseline/25.4
  }
  
  # Find 1990 to 2019 mean (i.e., 30 year average) of the observations
  obsmean = mean(county.obs$inches[match(1990, county.obs$year): match(2019, county.obs$year)])
  
  # Calculate the linear regression to find the rate of change over the historic/projection period
  # That is 1979 to 2020
  linreg45 = lm(rcp45df$means~rcp45df$baseline)
  trend45.mean = linreg45$coefficients['rcp45df$baseline']
  
  linreg85 = lm(rcp85df$means~rcp85df$baseline)
  trend85.mean = linreg85$coefficients['rcp85df$baseline']
  
  day45mean = trend45.mean*2070 + linreg45$coefficients['(Intercept)']
  day85mean = trend85.mean*2070 + linreg85$coefficients['(Intercept)']
  dayincrease45 = day45mean - obsmean
  dayincrease85 = day85mean - obsmean
  hindcastincrease45 = day45mean - baseline
  hindcastincrease85 = day85mean - baseline
  
  yr2070ind = which(rcp45df$baseline == 2070)
  
  exptval = data.frame(chngercp45 = rcp45df$means[yr2070ind] - baseline, 
                       chngercp85 = rcp85df$means[yr2070ind] - baseline,
                       realrcp45 = rcp45df$means[yr2070ind], 
                       realrcp85 = rcp85df$means[yr2070ind],
                       baseline = baseline,
                       obsmean = obsmean,
                       day45mean = day45mean,
                       day85mean = day85mean,
                       trend45.mean = trend45.mean,
                       trend85.mean = trend85.mean,
                       obs2070incease45 = dayincrease45,
                       obs2070incease85 = dayincrease85,
                       hindcast2070increase45 = hindcastincrease45,
                       hindcast2070increase85 = hindcastincrease85,
                       percenttrendincrease45 = (hindcastincrease45/baseline)*100,
                       percenttrendincrease85 = (hindcastincrease85/baseline)*100)
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
                rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
    
    # Transparent colors with transparent color function
    trans_colors100 = makeTransparent(colors, 100)
    trans_colors150 = makeTransparent(colors, 150)
    trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
    
    # Constructing graph ------------------------------------------------------
    if(is.null(season)){
      cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                      "-", fips, "-", var, "-precipHistgraph1.eps"), 
               width=5.72, height=4.04)
    } else {
      folderpath = path.expand(paste0(outDir, season, "/"))
      if(!dir.exists(folderpath)){ 
        dir.create(folderpath, recursive=T)
      }
      cairo_ps(paste0(folderpath, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                      "-", fips, "-", var, "-", season, "-precipHistgraph1.eps"), 
               width=5.72, height=4.04)
    }
    
    par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
    
    vioplot(inches~decade, data=rcp45Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[3], 7)),
            plotCentre = "line", side="left", xlab="", ylab=ylabel, xaxt="n", 
            ylim=range(c(county.obs$inches, rcp45Tab$inches, rcp85Tab$inches, na.rm=TRUE)))
    
    vioplot(inches~decade, data=rcp85Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[4], 7)), 
            plotCentre = "line", side = "right", add = T)
    
    stripchart(county.obs$inches~county.obs$decade, vertical=TRUE, 
               method = "jitter", pch = 20, add = TRUE, col=colors[6])
    
    axis(side=1, at=1:length(rcp45df$baseline), las = 2, label=rcp45df$baseline, tick=FALSE)

    ind = which(rcp45df$baseline %in% seq(2010, 2080, by=10))
    points(ind, rcp45df$means[ind], pch = 19, col = colors[1])
    points(ind, rcp85df$means[ind], pch = 19, col = colors[2])
    
    # Constructing Legend -----------------------------------------------------
    scenarios <- c("Hindcast Range", "Observations", 
                   "Low Emissions Average", "High Emissions Average",
                   "Low Emissions Range", "High Emissions Range")
    
    # Plot legend inside the graph or in the outer margin
    if(add.legend){
      if(leg.inside){
        legend("topleft", legend = scenarios,
               pch = c(19, 19, 15, 15, 15, 20), pt.cex = c(1, 1, 2, 2, 2, 1),
               col = c(colors[1:2], trans_colors[3:5], colors[6]), bty="n")
      } else{
        # For Average
        add_legend("topright", legend = scenarios,
                   pch = c(15, 20, 19, 19, 15, 15), pt.cex = c(2, 1, 1, 1, 2, 2),
                   col = c(trans_colors[5], colors[6], colors[1:2], trans_colors[3:4]),
                   bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))
      }
    }
      dev.off()
  }
  
  return(exptval)
}

##########################################################################
# Precipitation Change Bar Graph
##########################################################################
calcPrecipChangeBarGraph <- function(fips, dataTab, outDir, var, season = NULL, ylabel, 
                                     create.plot=TRUE, leg.inside = TRUE){
  #################
  # fips <- wet2indays$FIPS[1]
  # dataTab <- wet2indays
  # ylabel = "Percent Change in days with\nprecipitation ≥ 2in"
  # outDir <- precipDir
  # season = NULL
  # var = "2in"
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  
  ##Loop to class each observation by decade, then appending as an extra column
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  decades <- unique(dataTab$decade)
  
  ##Calculations to determine baseline precip levels from 1950-1980
  ## Do most recent decade 1990 - 2019
  # tar.decades <- c(1960, 1970, 1980) ###CODE TO SET CHRONOLOGICAL BASELINE
  tar.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- sum(baseSelect$wetDays) / nrow(baseSelect)
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  
  ##Loop to calculate relative change for each decade and RCP 4.5, compared to baseline
  county.rcp45.avg <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  ##Loop to calculate relative change for each decade and RCP 8.5, compared to baseline
  county.rcp85.avg <- sapply(decades, function(dec){decTab<-rcp85Tab[rcp85Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  ## Determine standard deviation
  county.rcp45.sd <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSd<-sd((decTab$wetDays - baseline)/baseline);
  return(decSd)})
  
  county.rcp85.sd <- sapply(decades, function(dec){decTab<-rcp85Tab[rcp85Tab$decade==dec,];
  decSd<-sd((decTab$wetDays - baseline)/baseline);
  return(decSd)})
  
  ##Assembling into a data frame for plotting
  graphing.data <- data.frame(rbind(county.rcp45.avg, county.rcp85.avg, 
                                    c(county.rcp45.avg[1:5], 
                                      mean(c(county.rcp45.avg[6], 
                                             county.rcp85.avg[6])), 
                                      rep(NA, times = 8))))
  graphing.data[1, 1:6] <- rep(NA, times= 6)
  graphing.data[2, 1:6] <- rep(NA, times= 6)
  colnames(graphing.data) <- as.character(decades)
  rownames(graphing.data) <- c("rcp45", "rcp85", "hindcast")
  
  graphing.data <- as.matrix(graphing.data)
  graphing.data <- graphing.data[,which(colnames(graphing.data)==1960):which(colnames(graphing.data)==2070)]
  
  ##Assembling SD for plotting
  sd.data <- data.frame(rbind(county.rcp45.sd, county.rcp85.sd, 
                              c(county.rcp45.sd[1:5], 
                                mean(c(county.rcp45.sd[6], 
                                       county.rcp85.sd[6])), 
                                rep(NA, times = 8))))
  sd.data[1, 1:6] <- rep(NA, times= 6)
  sd.data[2, 1:6] <- rep(NA, times= 6)
  colnames(sd.data) <- as.character(decades)
  rownames(sd.data) <- c("rcp45", "rcp85", "hindcast")
  
  sd.data <- as.matrix(sd.data)
  sd.data <- sd.data[,which(colnames(sd.data)==1960):which(colnames(sd.data)==2070)]
  
  # Export values -----------------------------------------------------------
  
  # Calculate the linear regression to find the rate of change over the historic period
  # That is 1979 to 2019
  rcp45.time = c(graphing.data['hindcast', which(colnames(graphing.data)==1960):which(colnames(graphing.data)==2000)],
  graphing.data['rcp45', which(colnames(graphing.data)==2010):which(colnames(graphing.data)==2070)])*100
  rcp85.time = c(graphing.data['hindcast', which(colnames(graphing.data)==1960):which(colnames(graphing.data)==2000)],
    graphing.data['rcp85', which(colnames(graphing.data)==2010):which(colnames(graphing.data)==2070)])*100
  
  linreg45 = lm(rcp45.time~c(1:length(rcp45.time)))
  trend45 = linreg45$coefficients['c(1:length(rcp45.time))']
  
  linreg85 = lm(rcp85.time~c(1:length(rcp85.time)))
  trend85 = linreg85$coefficients['c(1:length(rcp85.time))']
  
  # Calculate # of days increase over baseline
  time.ind = which(names(rcp85.time)=="2070")
  increase45 = trend45*time.ind + linreg45$coefficients['(Intercept)']
  increase85 = trend85*time.ind + linreg85$coefficients['(Intercept)']
  
  #To calculate percent-increase by 2070 and insert in Outlook text. 
  #We'll estimate conservatively, using RCP 4.5 and rounding down to
  #nearest multiple of 5
  exptval = data.frame(trend45 = trend45,
                       trend85 = trend85, 
                       increase45 = increase45,
                       increase85 = increase85,
                       chngercp45 = graphing.data['rcp45','2070']*100,
                       chngercp85 = graphing.data['rcp85','2070']*100,
                       realrcp45 = (graphing.data['rcp45','2070']*baseline)+baseline,
                       realrcp85 = (graphing.data['rcp85','2070']*baseline)+baseline,
                       baseline = baseline)
  # exptval = data.frame(chngercp45 = round_any(graphing.data['rcp45','2070']*100, 5, floor),
  #                      chngercp85 = round_any(graphing.data['rcp85','2070']*100, 5, floor),
  #                      realrcp45 = round(graphing.data['rcp45','2070']*baseline),
  #                      realrcp85 = round(graphing.data['rcp85','2070']*baseline))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    colors = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
    decades <- paste0(colnames(graphing.data), " ")
    scenarios <- c("Low Emissions Average", "High Emissions Average", "Hindcast Average")
    
    countynme <- unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1))
    ##Plotting
    if(is.null(season)){
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-precipchangeBargraph1.eps"), 
               width=5.72, height=4.04)
    } else {
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-", season, "-precipnochangeBargraph1.eps"), 
               width=5.72, height=4.04)
    }
    par(mar=c(3, 5, 2, 1), las=1)
    
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, -1)
    maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, 0)
    minVal <- round(min(graphing.data[3, ], na.rm=T)*100, 1)
    
    ##make the bar plot
    #Might need to adjust y axis here and below, as needed
    pretty_axis = pretty(c(minVal, maxVal))
    barplot(graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "gray60", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    # abline(linreg45)
    # abline(linreg85)
    box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    mtext(side=1, at=c(0.55, 1.5, 2.45, 3.4, 4.35, 5.35,
                       6.3, 7.3, 8.3, 9.2, 10.2, 11.2), las = 2, decades)
    
    # Plot legend inside the graph or in the outer margin
    if(leg.inside){
      legend("topleft", legend = scenarios,
             fill = c(colors, "gray60"), bty="n")
    } else{
      add_legend("topright", legend = scenarios,
                 fill = c(colors, "gray60"),
                 bty='n', cex=0.8, ncol=3, text.width=c(0.45, 0.43, 0.35))
    }
    dev.off()
  }
  
  return(exptval)
}

##########################################################################
# Precipitation - as Polygon graph
##########################################################################
calcPrecipRunAveGraph <- function(fips, dataAveTab, dataTab, obsTab, outDir, 
                                  ylabel = NULL, season = NULL, var, create.plot=TRUE,
                                  leg.inside = FALSE){
  #################
  # fips <- wet2indays$FIPS[1]
  # dataAveTab = wet2inAve
  # dataTab=wet2indays
  # obsTab = wet2inObs
  # ylabel = "Number of days with\nprecipitation ≥ 2in"
  # var="2in"
  # outDir=precipDir
  # season = NULL
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  
  # Format observations -----------------------------------------------------
  county.obs <- obsTab[obsTab$FIPS==fips,]
  
  # If the observations have multiple stats, then select the correct column
  if(is.null(season)){
    obs.ind = 4
    maca.ind = 6
  } else if(season=="winter"){
    obs.ind = 4
    maca.ind = 6
  } else if(season=="spring"){
    obs.ind = 5
    maca.ind = 7
  } else if(season=="summer"){
    obs.ind = 6
    maca.ind = 8
  } else if(season=="fall"){
    obs.ind = 7
    maca.ind = 9
  } else {
    obs.ind = 4
    maca.ind = 6
  }
  
  # Define 5-yr running average means ---------------------------------------
  centDecs <- unique(dataAveTab$centralDecade)
  decadal45Tabs <- lapply(centDecs, function(cd){dataAveTab[dataAveTab$FIPS==fips & dataAveTab$centralDecade==cd & dataAveTab$rcp=="rcp45",]})
  decadal85Tabs <- lapply(centDecs, function(cd){dataAveTab[dataAveTab$FIPS==fips & dataAveTab$centralDecade==cd & dataAveTab$rcp=="rcp85",]})
  
  #annual means
  statMeans45 <- lapply(decadal45Tabs, function(st){colMeans(st[,7:ncol(st)])})
  statMeans85 <- lapply(decadal85Tabs, function(st){colMeans(st[,7:ncol(st)])})
  
  #column mins
  statMins45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMins85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  #column maxes 
  statMaxs45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){max(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMaxs85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){max(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  #constructing a years sequence
  tsData <- data.frame(years=seq(from=1960,to=2079,by=1), rcp45.means=unlist(statMeans45), 
                       rcp45.maxes=unlist(statMaxs45), rcp45.mins=unlist(statMins45), 
                       rcp85.means=unlist(statMeans85), rcp85.maxes=unlist(statMaxs85), 
                       rcp85.mins=unlist(statMins85))
  
  # Define model spread from yearly data ------------------------------------
  model = unique(dataTab$model)
  years = unique(dataTab$year)
  
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  tar.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- mean(baseSelect[,maca.ind])
  
  county45Tabs <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  wetDay45Tabs <- lapply(model, function(cd){table = data.frame(county45Tabs[county45Tabs$model == cd, ][,maca.ind], 
                                                                row.names=years); colnames(table) = cd; return(table)})
  names(wetDay45Tabs) <- model
  rcp45 <- do.call(cbind.data.frame, wetDay45Tabs)
  
  county85Tabs <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  wetDay85Tabs <- lapply(model, function(cd){table = data.frame(county85Tabs[county85Tabs$model == cd, ][,maca.ind], 
                                                                row.names=years); colnames(table) = cd; return(table)})
  names(wetDay85Tabs) <- model
  rcp85 <- do.call(cbind.data.frame, wetDay85Tabs)
  
  #annual means
  statMeans45 <- rowMeans(rcp45)
  statMeans85 <- rowMeans(rcp85)
  
  #column mins
  statMins45 <- apply(rcp45, 1, FUN = min) #unlist(lapply(1:nrow(rcp45), function(X){min(rcp45[X, ])}))
  statMins85 <- apply(rcp85, 1, FUN = min) # unlist(lapply(1:nrow(rcp85), function(X){min(rcp85[X, ])}))
  
  #column maxes 
  statMaxs45 <- apply(rcp45, 1, FUN = max) #unlist(lapply(1:nrow(rcp45), function(X){max(rcp45[X, ])}))
  statMaxs85 <- apply(rcp85, 1, FUN = max) #unlist(lapply(1:nrow(rcp85), function(X){max(rcp85[X, ])}))
  
  tsYrData = data.frame(year = years, rcp45.means = statMeans45, rcp85.means = statMeans85, 
                        rcp45.mins = statMins45, rcp85.mins = statMins85, 
                        rcp45.maxs = statMaxs45, rcp85.maxs = statMaxs85)
  
  pltYr2019 <- which(tsYrData$year==2019)
  pltYr2070 <- which(tsYrData$year==2079)
  
  # MACA historical is from 1950-2005. Projections are after 2005. Must take the mean of both
  histPoltly <- data.frame(year = tsYrData$year[1:pltYr2019],
                           means = ((tsYrData[1:pltYr2019,]$rcp45.means + tsYrData[1:pltYr2019,]$rcp85.means)/2),
                           mins = ((tsYrData[1:pltYr2019,]$rcp45.mins + tsYrData[1:pltYr2019,]$rcp85.mins)/2),
                           maxs = ((tsYrData[1:pltYr2019,]$rcp45.maxs + tsYrData[1:pltYr2019,]$rcp85.maxs)/2))
  predPoltly <- tsYrData[pltYr2019:pltYr2070,]
  
  # Convert mm to inches
  if(var == "tot"){
    predPoltly[ ,2:ncol(predPoltly)] = predPoltly[ ,2:ncol(predPoltly)]/25.4
    histPoltly[ ,2:ncol(histPoltly)] = histPoltly[ ,2:ncol(histPoltly)]/25.4
    tsData[ ,2:ncol(tsData)] = tsData[ ,2:ncol(tsData)]/25.4
    county.obs[ ,obs.ind] = county.obs[ ,obs.ind]/25.4
    rcp45 = rcp45/25.4
    rcp85 = rcp85/25.4
  }
  
  # Export values -----------------------------------------------------------
  # Find 1990 to 2019 mean (i.e., 30 year average) of the observations
  obsmean = mean(county.obs[ ,obs.ind][match(1990, county.obs$year): match(2019, county.obs$year)])
  
  # Calculate the linear regression to find the rate of change over the historic period
  # That is 1979 to 2020
  linreg = lm(county.obs[ ,obs.ind]~county.obs$year)
  trend = linreg$coefficients['county.obs$year']
  
  #To calculate # of days increase over a 40 year period (roughly 1979 to 2020) and insert in Outlook text. 
  #We'll estimate using RCP 4.5 and rounding to the nearest whole day
  day1979 = trend*1979 + linreg$coefficients['(Intercept)']
  day2020 = trend*2020 + linreg$coefficients['(Intercept)']
  dayincrease = day2020 - day1979
  # dayincrease = round(trend*40, 0)
  
  # Calculate the linear regression to find the rate of change over the historic/projection period
  # That is 1979 to 2020
  linreg45 = lm(tsData$rcp45.means~tsData$year)
  trend45.mean = linreg45$coefficients['tsData$year']
  
  linreg85 = lm(tsData$rcp85.means~tsData$year)
  trend85.mean = linreg85$coefficients['tsData$year']
  
  day45mean = trend45.mean*2070 + linreg45$coefficients['(Intercept)']
  day85mean = trend85.mean*2070 + linreg85$coefficients['(Intercept)']
  dayincrease45 = day45mean - obsmean
  dayincrease85 = day85mean - obsmean
  hindcastincrease45 = day45mean - baseline
  hindcastincrease85 = day85mean - baseline
  
  # Calculate the projection trends for each model and each rcp scenario
  # Are all positive trends?
  trends45 = lapply(X=1:ncol(rcp45), function(X){lm(rcp45[ ,X]~as.numeric(row.names(rcp45)))$coefficients[2]})
  trends45 <- unlist(trends45)
  numPos45 <- sum((trends45 > 0), na.rm = TRUE)
  
  trends85 = lapply(X=1:ncol(rcp85), function(X){lm(rcp85[ ,X]~as.numeric(row.names(rcp85)))$coefficients[2]})
  trends85 <- unlist(trends85)
  numPos85 <- sum((trends85 > 0), na.rm = TRUE)
  
  # Find index for the year 2070
  predyr2070 = match(2070, predPoltly$year)
  tsr2070 = match(2070, tsData$year)
  
  # Create dataframe of values to export
  exptval = data.frame(obsmean = obsmean,
                       hindcastbaseline = baseline,
                       obstrend = trend,
                       obsincease19792020 = dayincrease,
                       numpostrend45 = numPos45,
                       numpostrend85 = numPos85,
                       day45mean = day45mean,
                       day85mean = day85mean,
                       trend45.mean = trend45.mean,
                       trend85.mean = trend85.mean,
                       obs2070incease45 = dayincrease45,
                       obs2070incease85 = dayincrease85,
                       hindcast2070increase45 = hindcastincrease45,
                       hindcast2070increase85 = hindcastincrease85,
                       # rlt1960 = dayincrease,
                       # trend = round(trend, 2),
                       mean452070 = tsData$rcp45.means[tsr2070],
                       mean852070 = tsData$rcp85.means[tsr2070])
                       # mean452070 = round(tsData$rcp45.means[tsr2070],0),
                       # mean852070 = round(tsData$rcp85.means[tsr2070],0))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    # Set colors
    colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
                rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
    
    # Transparent colors with transparent color function
    trans_colors100 = makeTransparent(colors, 100)
    trans_colors150 = makeTransparent(colors, 150)
    trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
    
    # Set y limits
    ymaxtick <- max(c(predPoltly$rcp85.maxs, predPoltly$rcp45.maxs, predPoltly$rcp85.mins, 
                      predPoltly$rcp45.mins, histPoltly$rcp85.maxs, histPoltly$rcp45.maxs, 
                      histPoltly$rcp85.mins, histPoltly$rcp45.mins, county.obs[ ,obs.ind]), na.rm=TRUE)
    ymintick <- min(c(predPoltly$rcp85.maxs, predPoltly$rcp45.maxs, predPoltly$rcp85.mins, 
                      predPoltly$rcp45.mins, histPoltly$rcp85.maxs, histPoltly$rcp45.maxs, 
                      histPoltly$rcp85.mins, histPoltly$rcp45.mins, county.obs[ ,obs.ind]), na.rm=TRUE)
    
    # Constructing graph ------------------------------------------------------
    if(is.null(season)){
      cairo_ps(paste0(outDir, unique(sapply(strsplit(county45Tabs$county,"_"),"[[",1)), 
                      "-", fips, "-", var,"-precipgraph1Proj.eps"), width=5.72, height=4.04)
    } else{
      cairo_ps(paste0(outDir, unique(sapply(strsplit(county45Tabs$county,"_"),"[[",1)), 
                      "-", fips, "-", var,"-", season, "-precipgraph1Proj.eps"), width=5.72, height=4.04)
    }
    par(mar=c(3, 5, 2, 1))
    
    plot(0, type="n",xlab="", ylab=ylabel, xaxs="i", 
         ylim=c(ymintick,ymaxtick), xlim=c(1978.25,2079))
    
    polygon(y = c(predPoltly$rcp45.mins, rev(predPoltly$rcp45.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[3], border = NA)
    polygon(y = c(predPoltly$rcp85.mins, rev(predPoltly$rcp85.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[4], border = NA)
    
    polygon(y = c(histPoltly$mins, rev(histPoltly$maxs)), 
            x = c(histPoltly$year, rev(histPoltly$year)), 
            col = trans_colors[5], border = NA)
    
    yrind = match(2019, tsData$year)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp45.means[yrind:nrow(tsData)], col=colors[1], lwd=2)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp85.means[yrind:nrow(tsData)], col=colors[2], lwd=2)
    
    points(county.obs$year, county.obs[ ,obs.ind], pch=20, col = colors[6])
    
    # Constructing Legend -----------------------------------------------------
    scenarios <- c("Observations", "Hindcast Range", "Low Emissions Average", 
                   "High Emissions Average", "Low Emissions Range", 
                   "High Emissions Range")
    
    # Plot legend inside the graph or in the outer margin
    if(leg.inside){
      legend("topleft", legend = scenarios,
             pch = c(NA, NA, 15, 15, 15, 20), lty=c(2, 2, NA, NA, NA, NA),
             lwd=c(2, 2, NA, NA, NA, NA), pt.cex = c(NA, NA, 2, 2, 2, 1),
             col = c(colors[1:2], trans_colors[3:5], colors[6]), bty="n")
    } else{
      # For Average
      add_legend("topright", legend = scenarios,
                 pch = c(20, 15, NA, NA, 15, 15), lty=c(NA, NA, 1, 1, NA, NA),
                 lwd=c(NA, NA, 2, 2, NA, NA), pt.cex = c(1, 2, NA, NA, 2, 2),
                 col = c(colors[6], trans_colors[5], colors[1:2], trans_colors[3:4]),
                 bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))#c(0.47, 0.47, 0.43, 0.43, 0.35, 0.35))
    }
    
    dev.off()
  }
  return(exptval)
}

##########################################################################
# SEASONAL GRAPHS
##########################################################################

##########################################################################
# Seasonal Precipitation Violin Graph
##########################################################################
# Format data for seasonal violin graph -----------------------------------
formatViolinObs = function(dataTab, obsTab, season=NULL, fips){
  # Set average periods -----------------------------------------------------
  tar.decades <- c(1960, 1970, 1980) ###CODE TO SET CHRONOLOGICAL BASELINE
  past.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  mid.decades <- c(2020, 2030, 2040) ###CODE TO SET CHRONOLOGICAL BASELINE
  future.decades <- c(2050, 2060, 2070)
  
  if(season == "winter"){
    obsTab$wetDays = obsTab$winter
    dataTab$wetDays = dataTab$winter
    
    # Remove the first winter
    obsTab = obsTab[-which(obsTab$year %in% 1979), ]
    dataTab = dataTab[-which(dataTab$year %in% 1958), ]
    
  } else if(season == "spring"){
    obsTab$wetDays = obsTab$spring
    dataTab$wetDays = dataTab$spring
    
  } else if(season == "summer"){
    obsTab$wetDays = obsTab$summer
    dataTab$wetDays = dataTab$summer
    
  } else if(season == "fall"){
    obsTab$wetDays = obsTab$fall
    dataTab$wetDays = dataTab$fall
    
  } else{
    print("Season is not winter, spring, summer, or fall")
  }
  
  # Format observations -----------------------------------------------------
  county.obs <- obsTab[obsTab$FIPS==fips,]
  county.obs$decade <- round_any(county.obs$year, 10, floor)
  
  county.obs$period <- county.obs$decade 
  county.obs$period[which(county.obs$decade %in% tar.decades)] <- "1960-1989"
  county.obs$period[which(county.obs$decade %in% past.decades)] <- "1990-2019"
  county.obs$period[which(county.obs$decade %in% mid.decades)] <- "2020-2049"
  
  county.obs$inches <- county.obs$wetDays
  
  # Format hindcasts and projections ----------------------------------------
  ##Loop to class each observation by decade, then appending as an extra column
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  decades <- unique(dataTab$decade)
  years <- unique(dataTab$year)
  
  # ##Calculations to determine baseline precip levels from 1950-1980
  # baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  ##Calculations to determine baseline precip levels from 1950-1980
  baseSelect <- dataTab[which((dataTab$decade %in% past.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- mean(baseSelect$wetDays) #/ nrow(baseSelect) # Includes rcp45 and rcp85 but values should be the same
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  
  # Remove partial 30-yr periods (1950s and 2080s)
  rcp45Tab = rcp45Tab[-which(rcp45Tab$decade %in% 1950), ]
  rcp45Tab = rcp45Tab[-which(rcp45Tab$decade %in% 2080), ]
  rcp85Tab = rcp85Tab[-which(rcp85Tab$decade %in% 1950), ]
  rcp85Tab = rcp85Tab[-which(rcp85Tab$decade %in% 2080), ]
  
  rcp45Tab$period <- rcp45Tab$decade 
  rcp45Tab$period[which(rcp45Tab$decade %in% tar.decades)] <- "1960-1989"
  rcp45Tab$period[which(rcp45Tab$decade %in% past.decades)] <- "1990-2019"
  rcp45Tab$period[which(rcp45Tab$decade %in% mid.decades)] <- "2020-2049"
  rcp45Tab$period[which(rcp45Tab$decade %in% future.decades)] <- "2050-2079"
  
  rcp85Tab$period <- rcp85Tab$decade 
  rcp85Tab$period[which(rcp85Tab$decade %in% tar.decades)] <- "1960-1989"
  rcp85Tab$period[which(rcp85Tab$decade %in% past.decades)] <- "1990-2019"
  rcp85Tab$period[which(rcp85Tab$decade %in% mid.decades)] <- "2020-2049"
  rcp85Tab$period[which(rcp85Tab$decade %in% future.decades)] <- "2050-2079"
  
  rcp45Tab$inches <- rcp45Tab$wetDays
  rcp85Tab$inches <- rcp85Tab$wetDays
  
  county.rcp45.avg <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  dec = decades[-which(decades %in% c(1950, 2080))]
  
  rcp45Means <- sapply(dec, function(dec_yr){mean(rcp45Tab$inches[which(rcp45Tab$decade %in% dec_yr)])})
  rcp45df <- data.frame(means = rcp45Means, baseline = dec)
  
  rcp85Means <- sapply(dec, function(dec_yr){mean(rcp85Tab$inches[which(rcp85Tab$decade %in% dec_yr)])})
  rcp85df <- data.frame(means = rcp85Means, baseline = dec)
  return(list(rcp45df = rcp45df, rcp85df = rcp85df, baseline = baseline, county.obs = county.obs, 
              rcp85Tab = rcp85Tab, rcp45Tab = rcp45Tab))
}

# Seasonal Violin graph ---------------------------------------------------
calcSeasPrecipViolin <- function(fips, dataTab, dataAveTab=NULL, obsTab, outDir, var, percent = TRUE, 
                                 season = NULL, ylabel, create.plot=TRUE, 
                                 leg.inside = TRUE, smoothing=FALSE, save.plot=TRUE, 
                                 add.legend = TRUE){
  #################
  # fips <- wetseasdays$FIPS[1]
  # dataTab <- wetseasdays
  # obsTab = wetseasObs
  # ylabel = "Number of days with\nprecipitation ≥ 1in"
  # percent=FALSE
  # outDir=precipDir
  # season = NULL
  # var = "1in"
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  winter = formatViolinObs(dataTab, obsTab, season="winter", fips)
  spring = formatViolinObs(dataTab, obsTab, season="spring", fips)
  summer = formatViolinObs(dataTab, obsTab, season="summer", fips)
  fall = formatViolinObs(dataTab, obsTab, season="fall", fips)
  
  # Export values ----------------------------------------------------------- 
  
  # exptval = data.frame(chngercp45 = round((rcp45df$means[4] - baseline), 0), 
  #                      chngercp85 = round((rcp85df$means[4] - baseline), 0),
  #                      realrcp45 = rcp45df$means[4], 
  #                      realrcp85 = rcp85df$means[4],
  #                      baseline = baseline)
  
  # Plot --------------------------------------------------------------------
  ##Setting some graphical information
  colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
              rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
  
  # Transparent colors with transparent color function
  trans_colors100 = makeTransparent(colors, 100)
  trans_colors150 = makeTransparent(colors, 150)
  trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
  
  # Constructing graph ------------------------------------------------------
  cairo_ps(paste0(outDir, unique(sapply(strsplit(spring$rcp45Tab$county,"_"),"[[",1)), 
                  "-", fips, "-", var, "SeasonViolin.eps"), 
           width=8, height=6)
  
  # par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
  
  par(mgp=c(1.5,.5,0), mfrow=c(2,2), las=1)
  
  yrange = range(c(winter$county.obs$inches, winter$rcp45Tab$inches, winter$rcp85Tab$inches,
                   spring$county.obs$inches, spring$rcp45Tab$inches, spring$rcp85Tab$inches,
                   fall$county.obs$inches, fall$rcp45Tab$inches, fall$rcp85Tab$inches,
                   summer$county.obs$inches, summer$rcp45Tab$inches, summer$rcp85Tab$inches, 
                   na.rm=TRUE))
  
  # winter ------------------------------------------------------------------
  par(mgp=c(1.5,.5,0), mar=c(0.5, 4, 2.5, 0), las=1)
  vioplot(inches~decade, data=winter$rcp45Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[3], 7)),
          plotCentre = "line", side="left", xlab="", ylab="", xaxt="n", 
          ylim=yrange)
  vioplot(inches~decade, data=winter$rcp85Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[4], 7)), 
          plotCentre = "line", side = "right", add = T)
  stripchart(winter$county.obs$inches~winter$county.obs$decade, vertical=TRUE, 
             method = "jitter", pch = 20, add = TRUE, col=colors[6])
  #axis(side=1, at=1:length(winter$rcp45df$baseline), las = 2, label=winter$rcp45df$baseline, tick=FALSE)
  
  mtext(side=2, line=-2, ylabel, font=1, las=0, outer=TRUE)
  
  winterind = which(winter$rcp45df$baseline %in% seq(2010, 2080, by=10))
  points(winterind, winter$rcp45df$means[winterind], pch = 19, col = colors[1])
  points(winterind, winter$rcp85df$means[winterind], pch = 19, col = colors[2])
  
  legend("topleft", "Winter", bty="n")
  # spring ------------------------------------------------------------------
  par(mgp=c(1.5,.5,0), mar=c(0.5, 1.5, 2.5, 1), las=1)
  vioplot(inches~decade, data=spring$rcp45Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[3], 7)),
          plotCentre = "line", side="left", xlab="", ylab="", xaxt="n", 
          ylim=yrange)
  vioplot(inches~decade, data=spring$rcp85Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[4], 7)), 
          plotCentre = "line", side = "right", add = T)
  stripchart(spring$county.obs$inches~spring$county.obs$decade, vertical=TRUE, 
             method = "jitter", pch = 20, add = TRUE, col=colors[6])
  #axis(side=1, at=1:length(spring$rcp45df$baseline), las = 2, label=spring$rcp45df$baseline, tick=FALSE)
  
  springind = which(spring$rcp45df$baseline %in% seq(2010, 2080, by=10))
  points(springind, spring$rcp45df$means[springind], pch = 19, col = colors[1])
  points(springind, spring$rcp85df$means[springind], pch = 19, col = colors[2])
  
  legend("topleft", "Spring", bty="n")
  # summer ------------------------------------------------------------------
  par(mgp=c(1.5,.5,0), mar=c(3, 4, 0, 0), las=1)
  vioplot(inches~decade, data=summer$rcp45Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[3], 7)),
          plotCentre = "line", side="left", xlab="", ylab="", xaxt="n", 
          ylim=yrange)
  vioplot(inches~decade, data=summer$rcp85Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[4], 7)), 
          plotCentre = "line", side = "right", add = T)
  stripchart(summer$county.obs$inches~summer$county.obs$decade, vertical=TRUE, 
             method = "jitter", pch = 20, add = TRUE, col=colors[6])
  axis(side=1, at=1:length(summer$rcp45df$baseline), las = 2, label=summer$rcp45df$baseline, tick=FALSE)
  
  summerind = which(summer$rcp45df$baseline %in% seq(2010, 2080, by=10))
  points(summerind, summer$rcp45df$means[summerind], pch = 19, col = colors[1])
  points(summerind, summer$rcp85df$means[summerind], pch = 19, col = colors[2])
  
  legend("topleft", "Summer", bty="n")
  
  # fall --------------------------------------------------------------------
  par(mgp=c(1.5,.5,0), mar=c(3, 1.5, 0, 1), las=1)
  vioplot(inches~decade, data=fall$rcp45Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[3], 7)),
          plotCentre = "line", side="left", xlab="", ylab="", xaxt="n", 
          ylim=yrange)
  vioplot(inches~decade, data=fall$rcp85Tab, col = c(rep(trans_colors[5], 5), rep(trans_colors[4], 7)), 
          plotCentre = "line", side = "right", add = T)
  stripchart(fall$county.obs$inches~fall$county.obs$decade, vertical=TRUE, 
             method = "jitter", pch = 20, add = TRUE, col=colors[6])
  axis(side=1, at=1:length(fall$rcp45df$baseline), las = 2, label=fall$rcp45df$baseline, tick=FALSE)
  
  fallind = which(fall$rcp45df$baseline %in% seq(2010, 2080, by=10))
  points(fallind, fall$rcp45df$means[fallind], pch = 19, col = colors[1])
  points(fallind, fall$rcp85df$means[fallind], pch = 19, col = colors[2])
  
  legend("topleft", "Fall", bty="n")
  
  # Constructing Legend -----------------------------------------------------
  scenarios <- c("Hindcast Range", "Observations", 
                 "Low Emissions Average", "High Emissions Average",
                 "Low Emissions Range", "High Emissions Range")
  
  # Plot legend inside the graph or in the outer margin
  if(leg.inside){
    legend("topleft", legend = scenarios,
           pch = c(19, 19, 15, 15, 15, 20), pt.cex = c(1, 1, 2, 2, 2, 1),
           col = c(colors[1:2], trans_colors[3:5], colors[6]), bty="n")
  } else{
    # For Average
    add_legend("topright", legend = scenarios,
               pch = c(15, 20, 19, 19, 15, 15), pt.cex = c(2, 1, 1, 1, 2, 2),
               col = c(trans_colors[5], colors[6], colors[1:2], trans_colors[3:4]),
               bty='n', cex=1, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))
  }
  dev.off()
  
  # return(exptval)
}

##########################################################################
# Seasonal Precipitation Change Bar Graph
##########################################################################
# Format data for seasonal bar graph --------------------------------------
formatSeasBar <- function(dataTab, fips, season = NULL){
  
  if(season == "winter"){
    dataTab$wetDays = dataTab$winter
    
    # Remove the first winter
    dataTab = dataTab[-which(dataTab$year %in% 1958), ]
    
  } else if(season == "spring"){
    dataTab$wetDays = dataTab$spring
    
  } else if(season == "summer"){
    dataTab$wetDays = dataTab$summer
    
  } else if(season == "fall"){
    dataTab$wetDays = dataTab$fall
    
  } else{
    print("Season is not winter, spring, summer, or fall")
  }
  
  ##Loop to class each observation by decade, then appending as an extra column
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  decades <- unique(dataTab$decade)
  
  ##Calculations to determine baseline precip levels from 1950-1980
  ## Do most recent decade 1990 - 2019
  # tar.decades <- c(1960, 1970, 1980) ###CODE TO SET CHRONOLOGICAL BASELINE
  tar.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- sum(baseSelect$wetDays) / nrow(baseSelect)
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  
  ##Loop to calculate relative change for each decade and RCP 4.5, compared to baseline
  county.rcp45.avg <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  ##Loop to calculate relative change for each decade and RCP 8.5, compared to baseline
  county.rcp85.avg <- sapply(decades, function(dec){decTab<-rcp85Tab[rcp85Tab$decade==dec,];
  decSumm<-mean((decTab$wetDays - baseline)/baseline);
  return(decSumm)})
  
  ##Assembling into a data frame for plotting
  graphing.data <- data.frame(rbind(county.rcp45.avg, county.rcp85.avg, 
                                    c(county.rcp45.avg[1:5], 
                                      mean(c(county.rcp45.avg[6], 
                                             county.rcp85.avg[6])), 
                                      rep(NA, times = 8))))
  graphing.data[1, 1:6] <- rep(NA, times= 6)
  graphing.data[2, 1:6] <- rep(NA, times= 6)
  colnames(graphing.data) <- as.character(decades)
  rownames(graphing.data) <- c("rcp45", "rcp85", "hindcast")
  
  graphing.data <- as.matrix(graphing.data)
  graphing.data <- graphing.data[,which(colnames(graphing.data)==1960):which(colnames(graphing.data)==2070)]
  
  return(list(graphing.data = graphing.data, rcp85Tab = rcp85Tab, rcp45Tab = rcp45Tab, 
              baseline = baseline))
}

# Bar graph ---------------------------------------------------------------
calcSeasPrecipChangeBarGraph <- function(fips, dataTab, outDir, var, season = NULL, ylabel, 
                                         create.plot=TRUE, leg.inside = TRUE){
  #################
  # fips <- wetseasdays$FIPS[1]
  # dataTab <- wetseasdays
  # ylabel = "Percent Change in days with precipitation ≥ 1in"
  # outDir <- precipDir
  # season = NULL
  # var = "2in"
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  
  winter = formatSeasBar(dataTab, fips, season = "winter")
  spring = formatSeasBar(dataTab, fips, season = "spring")
  summer = formatSeasBar(dataTab, fips, season = "summer")
  fall = formatSeasBar(dataTab, fips, season = "fall")
  
  # Export values -----------------------------------------------------------
  
  #To calculate percent-increase by 2070 and insert in Outlook text. 
  #We'll estimate conservatively, using RCP 4.5 and rounding down to
  # #nearest multiple of 5
  # exptval = data.frame(chngercp45 = round_any(graphing.data['rcp45','2070']*100, 5, floor), 
  #                      chngercp85 = round_any(graphing.data['rcp85','2070']*100, 5, floor),
  #                      realrcp45 = round(graphing.data['rcp45','2070']*baseline), 
  #                      realrcp85 = round(graphing.data['rcp85','2070']*baseline))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    colors = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
    decades <- paste0(colnames(winter$graphing.data), " ")
    scenarios <- c("Low Emissions Average", "High Emissions Average", "Hindcast Average")
    
    countynme <- unique(sapply(strsplit(winter$rcp45Tab$county,"_"),"[[",1))
    ##Plotting
    cairo_ps(paste0(outDir, countynme, 
                    "-", fips, "-", var, "-Season-precipnochangeBargraph1.eps"), 
             width=8, height=6)
    par(mar=c(3, 5, 2, 1), las=1)
    
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, -1)
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, 0)
    # minVal <- round(min(graphing.data[3, ], na.rm=T)*100, 1)
    maxVal <- round(max(winter$graphing.data[1, ], winter$graphing.data[2, ], 
                        spring$graphing.data[1, ], spring$graphing.data[2, ],
                        summer$graphing.data[1, ], summer$graphing.data[2, ],
                        fall$graphing.data[1, ], fall$graphing.data[2, ], na.rm=T)*100, 0)
    minVal <- round(min(winter$graphing.data[3, ], spring$graphing.data[3, ], 
                        summer$graphing.data[3, ], fall$graphing.data[3, ], na.rm=T)*100, 1)
    
    ##make the bar plot
    #Might need to adjust y axis here and below, as needed
    par(mgp=c(1.5,.5,0), mfrow=c(2,2), las=1)
    pretty_axis = pretty(c(minVal, maxVal))
    
    # winter ------------------------------------------------------------------
    par(mgp=c(1.5,.5,0), mar=c(1, 4, 1.5, 0), las=1)
    barplot(winter$graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(winter$graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "gray60", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    # mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    
    mtext(side=2, line=-1.5, ylabel, font=1, las=0, outer=TRUE)
    
    legend("topleft", "Winter", bty="n")
    
    # spring ------------------------------------------------------------------
    par(mgp=c(1.5,.5,0), mar=c(1, 3, 1.5, 0.5), las=1)
    barplot(spring$graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(spring$graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "gray60", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    # mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    
    legend("topleft", "Spring", bty="n")
    
    # summer ------------------------------------------------------------------
    par(mgp=c(1.5,.5,0), mar=c(3, 4, 0, 0), las=1)
    barplot(summer$graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(summer$graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "gray60", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    # mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    mtext(side=1, at=c(0.55, 1.5, 2.45, 3.4, 4.35, 5.35,
                       6.3, 7.3, 8.3, 9.2, 10.2, 11.2), las = 2, decades)
    
    legend("topleft", "Summer", bty="n")
    
    # fall --------------------------------------------------------------------
    par(mgp=c(1.5,.5,0), mar=c(3, 3, 0, 0.5), las=1)
    barplot(fall$graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(fall$graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "gray60", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    # mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    mtext(side=1, at=c(0.55, 1.5, 2.45, 3.4, 4.35, 5.35,
                       6.3, 7.3, 8.3, 9.2, 10.2, 11.2), las = 2, decades)

    # Plot legend inside the graph or in the outer margin
    if(leg.inside){
      legend("topleft", legend = scenarios, title="Fall", title.adj=0.2,
             fill = c(colors, "gray60"), bty="n")
    } else{
      legend("topleft", "Fall", bty="n")
      add_legend("topright", legend = scenarios,
                 fill = c(colors, "gray60"),
                 bty='n', cex=0.8, ncol=3, text.width=c(0.45, 0.43, 0.35))
    }
    dev.off()
  }
  
  # return(exptval)
}

##########################################################################
# Seasonal Precipitation - as Polygon graph
##########################################################################
calcSeasPrecipRunAveGraph <- function(fips, dataAveTab, dataTab, obsTab, outDir, 
                                      ylabel = NULL, season = NULL, xaxis = TRUE, var, create.plot=TRUE,
                                      leg.inside = FALSE, seasonylims = TRUE){
  #################
  #fips <- wetdays$FIPS[1]
  #dataTab <- hotdaysyr
  #dataAveTab <- hotdays
  #obsTab <- hotObs
  #outDir <- tempDir
  #ylabel = NULL
  # season = NULL
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  
  # Format observations -----------------------------------------------------
  county.obs <- obsTab[obsTab$FIPS==fips,]
  
  # If the observations have multiple stats, then select the correct column
  if(is.null(season)){
    obs.ind = 4
    maca.ind = 6
  } else if(season=="winter"){
    obs.ind = 4
    maca.ind = 6
  } else if(season=="spring"){
    obs.ind = 5
    maca.ind = 7
  } else if(season=="summer"){
    obs.ind = 6
    maca.ind = 8
  } else if(season=="fall"){
    obs.ind = 7
    maca.ind = 9
  } else {
    obs.ind = 4
    maca.ind = 6
  }
  
  # Define 5-yr running average means ---------------------------------------
  centDecs <- unique(dataAveTab$centralDecade)
  decadal45Tabs <- lapply(centDecs, function(cd){dataAveTab[dataAveTab$FIPS==fips & dataAveTab$centralDecade==cd & dataAveTab$rcp=="rcp45",]})
  decadal85Tabs <- lapply(centDecs, function(cd){dataAveTab[dataAveTab$FIPS==fips & dataAveTab$centralDecade==cd & dataAveTab$rcp=="rcp85",]})
  
  #annual means
  statMeans45 <- lapply(decadal45Tabs, function(st){colMeans(st[,7:ncol(st)])})
  statMeans85 <- lapply(decadal85Tabs, function(st){colMeans(st[,7:ncol(st)])})
  
  #column mins
  statMins45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMins85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  #column maxes 
  statMaxs45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){max(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMaxs85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(7:ncol(st),function(col){max(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  #constructing a years sequence
  tsData <- data.frame(years=seq(from=1960,to=2079,by=1), rcp45.means=unlist(statMeans45), 
                       rcp45.maxes=unlist(statMaxs45), rcp45.mins=unlist(statMins45), 
                       rcp85.means=unlist(statMeans85), rcp85.maxes=unlist(statMaxs85), 
                       rcp85.mins=unlist(statMins85))
  
  # Define model spread from yearly data ------------------------------------
  model = unique(dataTab$model)
  years = unique(dataTab$year)
  
  county45Tabs <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  wetDay45Tabs <- lapply(model, function(cd){table = data.frame(county45Tabs[county45Tabs$model == cd, ][,maca.ind], 
                                                                row.names=years); colnames(table) = cd; return(table)})
  names(wetDay45Tabs) <- model
  rcp45 <- do.call(cbind.data.frame, wetDay45Tabs)
  
  county85Tabs <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  wetDay85Tabs <- lapply(model, function(cd){table = data.frame(county85Tabs[county85Tabs$model == cd, ][,maca.ind], 
                                                                row.names=years); colnames(table) = cd; return(table)})
  names(wetDay85Tabs) <- model
  rcp85 <- do.call(cbind.data.frame, wetDay85Tabs)
  
  #annual means
  statMeans45 <- rowMeans(rcp45)
  statMeans85 <- rowMeans(rcp85)
  
  #column mins
  statMins45 <- apply(rcp45, 1, FUN = min) #unlist(lapply(1:nrow(rcp45), function(X){min(rcp45[X, ])}))
  statMins85 <- apply(rcp85, 1, FUN = min) # unlist(lapply(1:nrow(rcp85), function(X){min(rcp85[X, ])}))
  
  #column maxes 
  statMaxs45 <- apply(rcp45, 1, FUN = max) #unlist(lapply(1:nrow(rcp45), function(X){max(rcp45[X, ])}))
  statMaxs85 <- apply(rcp85, 1, FUN = max) #unlist(lapply(1:nrow(rcp85), function(X){max(rcp85[X, ])}))
  
  tsYrData = data.frame(year = years, rcp45.means = statMeans45, rcp85.means = statMeans85, 
                        rcp45.mins = statMins45, rcp85.mins = statMins85, 
                        rcp45.maxs = statMaxs45, rcp85.maxs = statMaxs85)
  
  pltYr2019 <- which(tsYrData$year==2019)
  pltYr2070 <- which(tsYrData$year==2079)
  
  # MACA historical is from 1950-2005. Projections are after 2005. Must take the mean of both
  histPoltly <- data.frame(year = tsYrData$year[1:pltYr2019],
                           means = ((tsYrData[1:pltYr2019,]$rcp45.means + tsYrData[1:pltYr2019,]$rcp85.means)/2),
                           mins = ((tsYrData[1:pltYr2019,]$rcp45.mins + tsYrData[1:pltYr2019,]$rcp85.mins)/2),
                           maxs = ((tsYrData[1:pltYr2019,]$rcp45.maxs + tsYrData[1:pltYr2019,]$rcp85.maxs)/2))
  predPoltly <- tsYrData[pltYr2019:pltYr2070,]
  
  # Convert mm to inches
  if(var == "tot"){
    predPoltly[ ,2:ncol(predPoltly)] = predPoltly[ ,2:ncol(predPoltly)]/25.4
    histPoltly[ ,2:ncol(histPoltly)] = histPoltly[ ,2:ncol(histPoltly)]/25.4
    tsData[ ,2:ncol(tsData)] = tsData[ ,2:ncol(tsData)]/25.4
    county.obs[ ,obs.ind] = county.obs[ ,obs.ind]/25.4
    rcp45 = rcp45/25.4
    rcp85 = rcp85/25.4
  }
  
  # Export values -----------------------------------------------------------
  # Calculate the linear regression to find the rate of change over the historic period
  # That is 1979 to 2019
  linreg = lm(county.obs[ ,obs.ind]~county.obs$year)
  trend = linreg$coefficients['county.obs$year']
  
  #To calculate # of days increase over a 40 year period (roughly 1979 to 2019) and insert in Outlook text. 
  #We'll estimate using RCP 4.5 and rounding to the nearest whole day
  dayincrease = round(trend*40, 0)
  
  # Calculate the projection trends for each model and each rcp scenario
  # Are all positive trends?
  trends45 = lapply(X=1:ncol(rcp45), function(X){lm(rcp45[ ,X]~as.numeric(row.names(rcp45)))$coefficients[2]})
  trends45 <- unlist(trends45)
  numPos45 <- sum((trends45 > 0), na.rm = TRUE)
  
  trends85 = lapply(X=1:ncol(rcp85), function(X){lm(rcp85[ ,X]~as.numeric(row.names(rcp85)))$coefficients[2]})
  trends85 <- unlist(trends85)
  numPos85 <- sum((trends85 > 0), na.rm = TRUE)
  
  # Find 1990 to 2019 mean (i.e., 30 year average) of the observations
  obsmean = mean(county.obs[ ,obs.ind][match(1990, county.obs$year): match(2019, county.obs$year)])
  
  # Find index for the year 2070
  predyr2070 = match(2070, predPoltly$year)
  tsr2070 = match(2070, tsData$year)
  
  # Create dataframe of values to export
  exptval = data.frame(obsmean = round(obsmean,0),
                       numpostrend45 = numPos45,
                       numpostrend85 = numPos85,
                       # rlt1960 = dayincrease,
                       # trend = round(trend, 2),
                       mean452070 = round(tsData$rcp45.means[tsr2070],0),
                       mean852070 = round(tsData$rcp85.means[tsr2070],0))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    # Set colors
    colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
                rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
    
    # Transparent colors with transparent color function
    trans_colors100 = makeTransparent(colors, 100)
    trans_colors150 = makeTransparent(colors, 150)
    trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
    
    # Set y limits
    if(seasonylims){
      ylimits = range(c(county.obs$winter, county.obs$spring, county.obs$summer, county.obs$fall,
                        county45Tabs$winter, county45Tabs$spring, county45Tabs$summer, county45Tabs$fall,
                        county85Tabs$winter, county85Tabs$spring, county85Tabs$summer, county85Tabs$fall), 
                      na.rm=TRUE)
    } else {
      ymaxtick <- max(c(predPoltly$rcp85.maxs, predPoltly$rcp45.maxs, predPoltly$rcp85.mins,
                        predPoltly$rcp45.mins, histPoltly$rcp85.maxs, histPoltly$rcp45.maxs,
                        histPoltly$rcp85.mins, histPoltly$rcp45.mins, county.obs[ ,obs.ind]), na.rm=TRUE)
      ymintick <- min(c(predPoltly$rcp85.maxs, predPoltly$rcp45.maxs, predPoltly$rcp85.mins,
                        predPoltly$rcp45.mins, histPoltly$rcp85.maxs, histPoltly$rcp45.maxs,
                        histPoltly$rcp85.mins, histPoltly$rcp45.mins, county.obs[ ,obs.ind]), na.rm=TRUE)
      ylimits=c(ymintick,ymaxtick)
    }
    
    # Constructing graph ------------------------------------------------------
    if(xaxis){
      plot(0, type="n",xlab="", ylab=ylabel, xaxs="i", 
           ylim=ylimits, xlim=c(1978.25,2079))
    } else {
      plot(0, type="n",xlab="", xaxt="n", ylab=ylabel, xaxs="i", 
           ylim=ylimits, xlim=c(1978.25,2079))
    }
    
    polygon(y = c(predPoltly$rcp45.mins, rev(predPoltly$rcp45.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[3], border = NA)
    polygon(y = c(predPoltly$rcp85.mins, rev(predPoltly$rcp85.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[4], border = NA)
    
    polygon(y = c(histPoltly$mins, rev(histPoltly$maxs)), 
            x = c(histPoltly$year, rev(histPoltly$year)), 
            col = trans_colors[5], border = NA)
    
    yrind = match(2019, tsData$year)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp45.means[yrind:nrow(tsData)], col=colors[1], lwd=2)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp85.means[yrind:nrow(tsData)], col=colors[2], lwd=2)
    
    points(county.obs$year, county.obs[ ,obs.ind], pch=20, col = colors[6])
  }
  return(exptval)
}

##########################################################################
# Plot seasonal cycle
##########################################################################
calcSeasCycle <- function(fips, pastdat, futdat, type, outDir, ylabel = NULL){
  #################
  # pastdat = cycletot1950
  # futdat = cycletot2020
  # fips=pastdat$FIPS[1]
  # type="tot"
  # ylabel = "Total precipitation (in)"
  # outDir <- precipDir
  #################

listmon = tolower(month.abb)

rcp45Tab <- lapply(listmon, function(mon){pastdat[pastdat$FIPS==fips & pastdat$months==mon & pastdat$rcp=="rcp45",]})
Tab45 <- lapply(rcp45Tab, function(dat){mean(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})
Tab45max <- lapply(rcp45Tab, function(dat){max(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})
Tab45min <- lapply(rcp45Tab, function(dat){min(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})

rcp85Tab <- lapply(listmon, function(mon){pastdat[pastdat$FIPS==fips & pastdat$months==mon & pastdat$rcp=="rcp85",]})
Tab85 <- lapply(rcp85Tab, function(dat){mean(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})
Tab85max <- lapply(rcp85Tab, function(dat){max(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})
Tab85min <- lapply(rcp85Tab, function(dat){min(rowMeans(dat[ ,which(colnames(dat) == 'X1950'):which(colnames(dat) == 'X1999')]))})

rcp45Tab2020 <- lapply(listmon, function(mon){futdat[futdat$FIPS==fips & futdat$months==mon & futdat$rcp=="rcp45",]})
Tab452020 <- lapply(rcp45Tab2020, function(dat){mean(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})
Tab452020max <- lapply(rcp45Tab2020, function(dat){max(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})
Tab452020min <- lapply(rcp45Tab2020, function(dat){min(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})

rcp85Tab2020 <- lapply(listmon, function(mon){futdat[futdat$FIPS==fips & futdat$months==mon & futdat$rcp=="rcp85",]})
Tab852020 <- lapply(rcp85Tab2020, function(dat){mean(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})
Tab852020max <- lapply(rcp85Tab2020, function(dat){max(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})
Tab852020min <- lapply(rcp85Tab2020, function(dat){min(rowMeans(dat[ ,which(colnames(dat) == 'X2020'):which(colnames(dat) == 'X2069')]))})

monthly = data.frame(rcp45_1950 = unlist(Tab45), rcp85_1950 = unlist(Tab85), rcp45_2020 = unlist(Tab452020), rcp85_2020 = unlist(Tab852020),
                     rcp45_1950max = unlist(Tab45max), rcp85_1950max = unlist(Tab85max), 
                     rcp45_2020max = unlist(Tab452020max), rcp85_2020max = unlist(Tab852020max),
                     rcp45_1950min = unlist(Tab45min), rcp85_1950min = unlist(Tab85min), 
                     rcp45_2020min = unlist(Tab452020min), rcp85_2020min = unlist(Tab852020min))

if(type=="tot"){
  monthly = monthly/25.4
}

# Set colors
colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
            rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")

# Transparent colors with transparent color function
trans_colors100 = makeTransparent(colors, 100)
trans_colors150 = makeTransparent(colors, 150)
trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])

# Set y limits
ylimits = range(monthly)

# Constructing graph ------------------------------------------------------
cairo_ps(paste0(outDir, unique(rcp45Tab[[1]]$county), "-", type,"-cycle.eps"), width=5.72, height=4.04)

par(mgp=c(1.5,.5,0), mar=c(3, 3, 2, 1))

plot(1:12, type="n",xlab="", ylab=ylabel, xaxs="i", ylim=ylimits, xaxt="n")

polygon(y = c(monthly$rcp45_2020min, rev(monthly$rcp45_2020max)), x = c(1:12, 12:1), col = trans_colors[3], border = NA)
polygon(y = c(monthly$rcp85_2020min, rev(monthly$rcp85_2020max)), x = c(1:12, 12:1), col = trans_colors[4], border = NA)

polygon(y = c(monthly$rcp45_1950min, rev(monthly$rcp45_1950max)), 
        x = c(1:12, 12:1), col = trans_colors[5], border = NA)

lines(1:12, monthly$rcp45_2020, col=colors[1], lwd=2)
lines(1:12, monthly$rcp85_2020, col=colors[2], lwd=2)
lines(1:12, monthly$rcp45_1950, col=colors[6], lwd=2)

#Adding x and y axes text
axis(side = 1, at = 1:12, labels = month.abb, cex=0.8)

# Constructing Legend -----------------------------------------------------
scenarios <- c("Hindcast Average", "Hindcast Range", "Low Emissions Average", 
               "High Emissions Average", "Low Emissions Range", 
               "High Emissions Range")

# For Average
add_legend("topright", legend = scenarios,
           pch = c(NA, 15, NA, NA, 15, 15), lty=c(1, NA, 1, 1, NA, NA),
           lwd=c(2, NA, 2, 2, NA, NA), pt.cex = c(NA, 2, NA, NA, 2, 2),
           col = c(colors[6], trans_colors[5], colors[1:2], trans_colors[3:4]),
           bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))#c(0.47, 0.47, 0.43, 0.43, 0.35, 0.35))


dev.off()
}


##########################################################################
# Plot seasonal cycle
##########################################################################
calcIDF <- function(fips, changeFactor, idf, rp, outDir){

  #################
  # fips = 51155
  # idf = idfPulaski
  # rp = c(2, 5, 10, 25, 50, 100)
  # changeFactor
  #outDir <- tempDir
  #################
  
  rcp45Tab <- changeFactor[changeFactor$FIPS==fips & changeFactor$rcp=="rcp45",]
  factor45 <- colMeans(rcp45Tab[ ,5:ncol(rcp45Tab)])
  
  rcp85Tab <- changeFactor[changeFactor$FIPS==fips & changeFactor$rcp=="rcp85",]
  factor85 <- colMeans(rcp85Tab[ ,5:ncol(rcp85Tab)])
  
  subidf <- idf[, c(1, which(colnames(idf) %in% paste0("X", rp)))]
  
  idf45 <- lapply(rp, function(returnperiod){subidf[ ,which(colnames(subidf) == paste0("X", returnperiod))] * 
      factor45[which(names(factor45)==paste0("X", returnperiod, "yr"))]})
  idf45_df <- cbind.data.frame(idf45)
  colnames(idf45_df) = paste0("X", rp)
  rownames(idf45_df) = gsub(":", "", subidf$by.duration.for.ARI..years..)
  
  idf85 <- lapply(rp, function(returnperiod){subidf[ ,which(colnames(subidf) == paste0("X", returnperiod))] * 
      factor85[which(names(factor85)==paste0("X", returnperiod, "yr"))]})
  idf85_df <- cbind.data.frame(idf85)
  colnames(idf85_df) = paste0("X", rp)
  rownames(idf85_df) = gsub(":", "", subidf$by.duration.for.ARI..years..)
  
  # Duration in minutes
  # 5-min, 10-min, 15-min, 30-min, 60-min, 2-hr, 3-hr, 6-hr, 12-hr
  # 24-hr, 2-day, 3-day, 4-day, 7-day, 10-day, 20-day, 30-day, 45-day, 60-day
  duration = c(5, 10, 15, 30, 60, 120, 180, 360, 720, 1440, 2880, 4320, 5760, 
               10080, 14400, 28800, 43200, 64800, 86400)
  durlab = c("5-min", "10-min", "15-min", "30-min", "60-min", "2-hr", "3-hr", 
             "6-hr", "12-hr", "24-hr", "2-day", "3-day", "4-day", "7-day", 
             "10-day", "20-day", "30-day", "45-day", "60-day")
  
  ylimits = range(subidf[1:which(subidf[ ,1]=="24-hr:"),2:ncol(subidf)], 
                  idf45_df[1:which(rownames(idf45_df)=='24-hr'), ], 
                  idf85_df[1:which(rownames(idf85_df)=='24-hr'), ])
  xlimits = c(5, 1440)
  
  # Set colors
  colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
              rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
  colorhind = brewer.pal((length(rp)+1), "Greys")[2:(length(rp)+1)]
  color45 = brewer.pal((length(rp)+1), "Blues")[2:(length(rp)+1)]
  color85 = brewer.pal((length(rp)+1), "Reds")[2:(length(rp)+1)]
  
  namerp = paste0(rp, "yr")
  
  cairo_ps(paste0(outDir, unique(rcp45Tab$county), "-IDF.eps"), width=5.72, height=4.04)
  par(mgp=c(1.5,.5,0), mar=c(3, 3, 1, 1))
  
  plot(duration, type="n", xlab="Duration", ylab="Depth (in)", ylim=ylimits, 
       xlim = xlimits, xaxt="n", las=1, log="x")
  
  for(i in 1:length(rp)){
    lines(duration, subidf[ ,which(colnames(subidf) == paste0("X", rp[i]))], col=colors[6], lwd=2)
    lines(duration, idf45_df[ ,which(colnames(idf45_df) == paste0("X", rp[i]))], col=colors[1], lwd=2)
    lines(duration, idf85_df[ ,which(colnames(idf85_df) == paste0("X", rp[i]))], col=colors[2], lwd=2)
    text(duration[10], idf85_df[ ,which(colnames(idf85_df) == paste0("X", rp[i]))][10], col=colors[2], 
           labels = namerp[i], cex=0.9)
    text(duration[9], idf45_df[ ,which(colnames(idf45_df) == paste0("X", rp[i]))][9], col=colors[1], 
         labels = namerp[i], cex=0.9)
    text(duration[8], subidf[ ,which(colnames(subidf) == paste0("X", rp[i]))][8], col=colors[6], 
         labels = namerp[i], cex=0.9)
  }

  #Adding x and y axes text
  axis(side = 1, at = duration, labels = durlab, cex=0.8)

  scenarios <- c("Low Emissions Average", "High Emissions Average", "Hindcast Average")
  legend("topleft", legend = scenarios,
         fill = c(colors, "gray60"), bty="n")
  dev.off()
}

##########################################################################
# END
##########################################################################

