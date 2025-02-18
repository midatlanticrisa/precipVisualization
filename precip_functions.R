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
# Precipitation Box plot Graph
##########################################################################
calcPrecipBox <- function(fips, dataTab, obsTab, outDir, var, 
                          season = NULL, ylabel, save.plot=TRUE,
                             create.plot=TRUE, add.legend = TRUE){
  #################
  # fips <- "shp"
  # dataTab=NHwet99thdays
  # obsTab = NHwet99thObs
  # ylabel = "Number of 1% precipitation days"
  # var="99thdays"
  # season = NULL
  # outDir=NHprecipDir
  # create.plot=TRUE
  # add.legend = TRUE
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

  if(var == "tot" | var == "99tot"){
    county.obs$inches <- county.obs$wetDays/25.4
  } else {
    county.obs$inches <- county.obs$wetDays
  }
  
  ##Loop to calculate values for each observation decade 
  obsdecade = unique(county.obs$decade)
  obsdecade.avg <- sapply(obsdecade, function(dec){decTab<-county.obs[county.obs$decade==dec,];
  decSumm<-mean(decTab$inches);
  return(decSumm)})
  
  obs.avg = data.frame(decade = obsdecade, obs = obsdecade.avg)
  
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
  
  if(var == "tot" | var == "99tot"){
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
  
  rcp45_20502079 = mean(rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")])
  rcp85_20502079 = mean(rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")])
  medrcp45_20502079 = median(rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")])
  medrcp85_20502079 = median(rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")])
  
  # Export values ----------------------------------------------------------- 
  # round to nearest inch
  if(var == "tot" | var == "99tot"){
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
  
  # What is the change between vals in 2050-2070 vs 30-year observation mean
  val.increase45 = rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")] - obsmean
  val.increase85 = rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")] - obsmean
  
  # What is the percent with values above zero?
  likl.increase45 = (length(which(val.increase45 > 0))/length(val.increase45))*100
  likl.increase85 = (length(which(val.increase85 > 0))/length(val.increase85))*100
  
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
                       percenttrendincrease85 = (hindcastincrease85/baseline)*100,
                       rcp45_20502079 = rcp45_20502079,
                       rcp85_20502079 = rcp85_20502079,
                       obs2070incease45 = rcp45_20502079 - obsmean, 
                       obs2070incease85 = rcp85_20502079 - obsmean, 
                       percentobsincrease45 = ((rcp45_20502079 - obsmean)/obsmean)*100, 
                       percentobsincrease85 = ((rcp85_20502079 - obsmean)/obsmean)*100,
                       likl.increase45 = likl.increase45, 
                       likl.increase85 = likl.increase85,
                       medrcp45_20502079 = medrcp45_20502079,
                       medrcp85_20502079 = medrcp85_20502079,
                       medobs2070incease45 = medrcp45_20502079 - obsmean, 
                       medobs2070incease85 = medrcp85_20502079 - obsmean, 
                       medpercentobsincrease45 = ((medrcp45_20502079 - obsmean)/obsmean)*100, 
                       medpercentobsincrease85 = ((medrcp85_20502079 - obsmean)/obsmean)*100)
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
                rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
    
    # Transparent colors with transparent color function
    trans_colors100 = makeTransparent(colors, 100)
    trans_colors150 = makeTransparent(colors, 150)
    trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
    # 
    # # Constructing graph ------------------------------------------------------
    # if(is.null(season)){
    #   # cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
    #   #                 "-", fips, "-", var, "-precipBoxgraph1.eps"), 
    #   #          width=5.72, height=4.04)
    # } else {
    #   folderpath = path.expand(paste0(outDir, season, "/"))
    #   if(!dir.exists(folderpath)){ 
    #     dir.create(folderpath, recursive=T)
    #   }
    #   cairo_ps(paste0(folderpath, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
    #                   "-", fips, "-", var, "-", season, "-precipBoxgraph1.eps"), 
    #            width=5.72, height=4.04)
    # }
    
    # if(add.legend){
    #   par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
    # } else {
    #   par(mgp=c(1.5,.5,0), mar=c(3, 4, 0.5, 1), las=1)
    # }
    
    # allvals = c(county.obs$inches, rcp45Tab$inches, rcp85Tab$inches)
    
    ##Assembling into a data frame for plotting
    hindAve = rep(NA, nrow(rcp45Tab))
    for(i in 1:nrow(rcp45Tab)){
      hindAve[i] = mean(rcp45Tab$inches[i], rcp85Tab$inches[i])
    }
    hindTab = data.frame(decade = rcp45Tab$decade, model = rcp45Tab$model, inches=hindAve)
    # subhind = hindTab[which(hindTab$decade %in% seq(1960, 2010, by=10)), ]
    subhind = hindTab[which(hindTab$decade %in% seq(1980, 2010, by=10)), ]
    subrcp45 = rcp45Tab[which(rcp45Tab$decade %in% seq(2020, 2070, by=10)), ]
    subrcp85 = rcp85Tab[which(rcp85Tab$decade %in% seq(2020, 2070, by=10)), ]

    datatest = data.frame(decade = c(subhind$decade, subrcp45$decade, subrcp85$decade), 
                          inches = c(subhind$inches, subrcp45$inches, subrcp85$inches),
                          type = c(rep("hindcast", nrow(subhind)), 
                                   rep("rcp45", nrow(subrcp45)),
                                   rep("rcp85", nrow(subrcp85))))
    
    df2 = datatest
    df2$type <- factor(df2$type, levels = c("hindcast","rcp45","rcp85"))
    df2$decade = as.character(df2$decade)
    
    # # Count the number of decades (for boxplot)
    # decadeind = which(decades %in% unique(county.obs$decade))
    # for(dind in decadeind){
    #   county.obs$int[which(county.obs$decade == decades[dind])] <- dind
    # }
    # county.obs$int = county.obs$int -1 # Chart starts at 1960 not 1950
    # county.obs$shape = "Observations" # for legend
    
    obs.avg$shape = "Observation average" # for legend
    
    # Show full decades (10 years of data) only 
    
    obs.avg = obs.avg[which(obs.avg$decade==1980):which(obs.avg$decade==2010), ]
    # Count the number of decades (for boxplot)
    decadeind = which(decades %in% unique(obs.avg$decade))
    for(dind in decadeind){
      obs.avg$int[which(obs.avg$decade == decades[dind])] <- dind
    }
    obs.avg$int = obs.avg$int -3 # Chart starts at 1960 not 1950
    
    # compute lower and upper whiskers limits
    ylim1 = boxplot.stats(df2$inches)$stats[c(1, 5)]
    
    # ylim2 = c(min(c(ylim1[1], county.obs$inches)), 
    #           max(c(county.obs$inches, ylim1[2]*1.2)))
    ylim2 = c(min(c(ylim1[1], obs.avg$obs)), 
              max(c(obs.avg$obs, ylim1[2]*1.2)))
    
    # geom_point(data=county.obs, aes(x=int, y=inches, shape = shape), color="black") +
# legend.margin=margin(0,0,0,-6), 
    
    # scale_y_continuous(values="black", 
    #                    labels='1990-2019 average') +
    # legend.margin=margin(0,-10,0,-10), 
    
    library(grid)
    # Create a text
    grob <- grobTree(textGrob("1990-2019 average", x=0.1,  y=0.95, hjust=0), gp=gpar(fontsize=10))#,
                              #gp=gpar(col="red", fontsize=13, fontface="italic")))
    grob2 <- grobTree(textGrob("- - -", x=0.025,  y=0.95, hjust=0), gp=gpar(fontsize=10))
    
    # Plot legend
    if(add.legend){
      bpl <- ggplot(data = df2, aes(x=decade,y=inches, color=type)) +
        stat_boxplot(geom = "errorbar")+
        scale_color_manual(values=rep("black", 3))+
        geom_boxplot(aes(fill=type), outlier.shape = NA) +
        theme_bw() + 
        theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) +
        theme(legend.position="top", legend.box.spacing = unit(0, "pt"),
              legend.margin=margin(0,0,0,0), 
              legend.spacing.x = unit(0.05, "cm"),
              legend.text=element_text(size=9),
              plot.margin = margin(2, 15, 2, 2),
              axis.title = element_text(size = 12, colour="black"),
              axis.text.y = element_text(size = 12, colour="black"),
              axis.text.x = element_text(size = 12, colour="black", angle=90)) +
        geom_hline(yintercept=obsmean, linetype="dashed") +
        labs(x="", y=ylabel, color="", fill="", shape="") + 
        guides(color="none") + 
        scale_fill_manual(values=c("#CCCCCC", "#9BC3D7", "#FFB0C3"), 
                          labels=c('Hindcast', 'Low emissions range', 'High emissions range')) +
        annotation_custom(grob) + annotation_custom(grob2) + 
        geom_point(data=obs.avg, aes(x=int, y=obs, shape = shape), color="black") +
        coord_cartesian(ylim = ylim2)
      # show(bpl)
# scale_fill_manual(values=c("#CCCCCC", "#99c1d6", "#ffadc1"), 
    } else {
      bpl <- ggplot(data = df2, aes(x=decade,y=inches, color=type)) +
        stat_boxplot(geom = "errorbar")+
        scale_color_manual(values=rep("black", 3))+
        geom_boxplot(aes(fill=type), outlier.shape = NA, show.legend = FALSE) +
        theme_bw() + 
        theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) +
        theme(axis.title = element_text(size = 14, colour="black"), 
              axis.text.y = element_text(size = 14, colour="black"),
              axis.text.x = element_text(size = 14, colour="black", angle=90)) +
        # geom_point(data=county.obs, aes(x=int, y=inches, shape = shape), 
        #            color="black", show.legend = FALSE) +
        geom_point(data=obs.avg, aes(x=decade, y=obs, shape = shape), 
                   color="black", show.legend = FALSE) +
        labs(x="", y=ylabel, color="", fill="", shape="") + 
        guides(color="none") +
        scale_fill_manual(values=c("#CCCCCC", "#9BC3D7", "#FFB0C3")) +
        coord_cartesian(ylim = ylim2)
      # print(bp)
    }
    # dev.off()
    if(save.plot){
    psname = paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)),
                    "-", fips, "-", var, "-precipBoxgraph1.eps")
    ggsave(filename=psname, plot=bpl, width=5.72, height=4.04, units="in")
    }
  }
  
  if(!save.plot){
    return(bpl)
  } else {
    return(exptval)
  }
}
##########################################################################
# Precipitation Obs Bar Graph
##########################################################################
calcPrecipObsBarGraph <- function(fips, dataTab, obsTab, outDir, var, season = NULL, ylabel, 
                               create.plot=TRUE, leg.inside = TRUE, add.legend=TRUE,
                               save.plot=TRUE){
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
  
  ## Do most recent decade 1990 - 2019
  past.decades <- c(1960, 1970, 1980) ###CODE TO SET CHRONOLOGICAL BASELINE
  tar.decades <- c(1990, 2000, 2010) ###CODE TO SET CHRONOLOGICAL BASELINE
  mid.decades <- c(2020, 2030, 2040)
  future.decades <- c(2050, 2060, 2070)
  
  county.obs <- obsTab[obsTab$FIPS==fips,]
  county.obs$decade <- round_any(county.obs$year, 10, floor)
  
  county.obs$period <- county.obs$decade 
  county.obs$period[which(county.obs$decade %in% past.decades)] <- "1960-1989"
  county.obs$period[which(county.obs$decade %in% tar.decades)] <- "1990-2019"
  county.obs$period[which(county.obs$decade %in% mid.decades)] <- "2020-2049"
  
  if(var == "tot" | var == "99tot"){
    county.obs$inches <- county.obs$wetDays/25.4
  } else {
    county.obs$inches <- county.obs$wetDays
  }
  
  ##Loop to calculate values for each observation decade 
  obsdecade = unique(county.obs$decade)
  obsdecade.avg <- sapply(obsdecade, function(dec){decTab<-county.obs[county.obs$decade==dec,];
  decSumm<-mean(decTab$inches);
  return(decSumm)})
  
  obs.avg = data.frame(decade = obsdecade, obs = obsdecade.avg)
  
  ##Loop to class each observation by decade, then appending as an extra column
  dataTab$decade <- round_any(dataTab$year, 10, floor)
  decades <- unique(dataTab$decade)
  
  ##Calculations to determine baseline precip levels from 1950-1980
  baseSelect <- dataTab[which((dataTab$decade %in% tar.decades) & dataTab$FIPS==fips),]
  
  ##Baseline calculation of wet days per model-year
  baseline <- sum(baseSelect$wetDays) / nrow(baseSelect)
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
  
  ##Loop to calculate values for each decade and RCP 4.5
  county.rcp45.avg <- sapply(decades, function(dec){decTab<-rcp45Tab[rcp45Tab$decade==dec,];
  decSumm<-mean(decTab$wetDays);
  return(decSumm)})
  
  ##Loop to calculate values for each decade and RCP 8.5
  county.rcp85.avg <- sapply(decades, function(dec){decTab<-rcp85Tab[rcp85Tab$decade==dec,];
  decSumm<-mean(decTab$wetDays);
  return(decSumm)})
  
  ## "2050-2079"
  rcp45Tab_ind = which(rcp45Tab$decade %in% future.decades)
  rcp85Tab_ind = which(rcp85Tab$decade %in% future.decades)
  
  rcp45_20502079 = mean(rcp45Tab$wetDays[rcp45Tab_ind])
  rcp85_20502079 = mean(rcp85Tab$wetDays[rcp85Tab_ind])
  
  ##Assembling into a data frame for plotting
  graphing.data <- data.frame(rbind(county.rcp45.avg, county.rcp85.avg, 
                                    c(county.rcp45.avg[1:5], 
                                      mean(c(county.rcp45.avg[6], 
                                             county.rcp85.avg[6])), 
                                      mean(c(county.rcp45.avg[7], 
                                             county.rcp85.avg[7])),
                                      rep(NA, times = 7))))
  graphing.data[1, 1:7] <- rep(NA, times= 7)
  graphing.data[2, 1:7] <- rep(NA, times= 7)
  colnames(graphing.data) <- as.character(decades)
  rownames(graphing.data) <- c("rcp45", "rcp85", "hindcast")
  
  graphing.data <- as.matrix(graphing.data)
  graphing.data <- graphing.data[,which(colnames(graphing.data)==1980):which(colnames(graphing.data)==2070)]
  
  # Show full decades (10 years of data) only 
  obs.avg = obs.avg[which(obs.avg$decade==1980):which(obs.avg$decade==2010), ]
  
  # Export values -----------------------------------------------------------
  # Find 1990 to 2019 mean (i.e., 30 year average) of the observations
  obsmean = mean(county.obs$inches[match(1990, county.obs$year): match(2019, county.obs$year)])
  
  # Calculate the linear regression to find the rate of change over the historic period
  # That is 1979 to 2019
  rcp45.time = c(graphing.data['hindcast', which(colnames(graphing.data)==1980):which(colnames(graphing.data)==2000)],
                 graphing.data['rcp45', which(colnames(graphing.data)==2010):which(colnames(graphing.data)==2070)]) - baseline
  rcp85.time = c(graphing.data['hindcast', which(colnames(graphing.data)==1980):which(colnames(graphing.data)==2000)],
                 graphing.data['rcp85', which(colnames(graphing.data)==2010):which(colnames(graphing.data)==2070)])- baseline
  
  linreg45 = lm(rcp45.time~c(1:length(rcp45.time)))
  trend45 = linreg45$coefficients['c(1:length(rcp45.time))']
  
  linreg85 = lm(rcp85.time~c(1:length(rcp85.time)))
  trend85 = linreg85$coefficients['c(1:length(rcp85.time))']
  
  # Calculate # of days increase over baseline
  time.ind = which(names(rcp85.time)=="2070")
  increase45 = trend45*time.ind + linreg45$coefficients['(Intercept)']
  increase85 = trend85*time.ind + linreg85$coefficients['(Intercept)']
  
  # What is the change between vals in 2050-2070 vs 30-year hindcast mean
  val.hind.increase45 = rcp45Tab$wetDays[rcp45Tab_ind] - baseline
  val.hind.increase85 = rcp85Tab$wetDays[rcp85Tab_ind] - baseline
  
  # What is the percent with values above zero?
  likl.hind.increase45 = (length(which(val.hind.increase45 > 0))/length(val.hind.increase45))*100
  likl.hind.increase85 = (length(which(val.hind.increase85 > 0))/length(val.hind.increase85))*100
  
  # What is the change between vals in 2050-2070 vs 30-year observation mean
  val.increase45 = rcp45Tab$wetDays[rcp45Tab_ind] - obsmean
  val.increase85 = rcp85Tab$wetDays[rcp85Tab_ind] - obsmean
  
  # What is the percent with values above zero?
  likl.increase45 = (length(which(val.increase45 > 0))/length(val.increase45))*100
  likl.increase85 = (length(which(val.increase85 > 0))/length(val.increase85))*100
  
  #To calculate percent-increase by 2070 and insert in Outlook text. 
  #We'll estimate conservatively, using RCP 4.5 and rounding down to
  #nearest multiple of 5
  exptval = data.frame(trend45 = trend45,
                       trend85 = trend85, 
                       increase45 = increase45,
                       increase85 = increase85,
                       chngercp45 = graphing.data['rcp45','2070'],
                       chngercp85 = graphing.data['rcp85','2070'],
                       realrcp45 = graphing.data['rcp45','2070']+baseline,
                       realrcp85 = graphing.data['rcp85','2070']+baseline,
                       baseline = baseline,
                       rcp45_20502079 = rcp45_20502079,
                       rcp85_20502079 = rcp85_20502079,
                       hindcast2070incease45 = rcp45_20502079 - baseline, 
                       hindcast2070incease85 = rcp85_20502079 - baseline, 
                       percenthindincrease45 = ((rcp45_20502079 - baseline)/baseline)*100, 
                       percenthindincrease = ((rcp85_20502079 - baseline)/baseline)*100,
                       obsmean = obsmean,
                       likl.hind.increase45 = likl.hind.increase45,
                       likl.hind.increase85 = likl.hind.increase85,
                       percentobsincrease45 = ((rcp45_20502079 - obsmean)/obsmean)*100, 
                       percentobsincrease85 = ((rcp85_20502079 - obsmean)/obsmean)*100,
                       likl.increase45 = likl.increase45, 
                       likl.increase85 = likl.increase85)
  # exptval = data.frame(chngercp45 = round_any(graphing.data['rcp45','2070']*100, 5, floor),
  #                      chngercp85 = round_any(graphing.data['rcp85','2070']*100, 5, floor),
  #                      realrcp45 = round(graphing.data['rcp45','2070']*baseline),
  #                      realrcp85 = round(graphing.data['rcp85','2070']*baseline))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    # colors = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
    colors = c("#9BC3D7", "#FFB0C3")
    decades <- paste0(colnames(graphing.data), " ")
    scenarios <- c("Low emissions average", "High emissions average", "Hindcast average")
    
    countynme <- unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1))
    ##Plotting
    if(save.plot){
    if(is.null(season)){
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-ObsprecipBargraph1.eps"), 
               width=5.72, height=4.04)
    } else {
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-", season, "-ObsprecipBargraph1.eps"), 
               width=5.72, height=4.04)
    }
    }
    if(!leg.inside){
      par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
    } else {
      par(mgp=c(1.5,.5,0), mar=c(3, 5, 0.5, 1), las=1)
    }
    
    
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, -1)
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, 0)
    # minVal <- round(min(graphing.data[3, ], na.rm=T)*100, 1)
    # allvals = round(c(graphing.data[1:3, ], county.obs$wetDays), 1)
    allvals = round(c(graphing.data[1:3, ], obs.avg$obs), 1)
    
    ##make the bar plot
    #Might need to adjust y axis here and below, as needed
    # start at 0 if its number of days
    if(var == "99thdays"){
      pretty_axis = pretty(c(0, max(allvals, na.rm=TRUE))) 
    } else {
      pretty_axis = pretty(c(min(allvals, na.rm=TRUE), max(allvals, na.rm=TRUE)))
    }
    
    barplot(graphing.data[1:2, ], beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4), xpd=FALSE)
    par(new = T)
    barplot(graphing.data[3, ], ylim = range(pretty_axis),
            col = "#CCCCCC", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F, xpd=FALSE)
    abline(h = baseline, lty = 2)
    # abline(linreg45)
    # abline(linreg85)
    box()

    points(c(0.55, 1.5, 2.45, 3.4), obs.avg$obs, pch=20, col = "black")
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, " "))
    mtext(side=2, line=2, at=mean(pretty_axis), ylabel, font=1, las=0)
    mtext(side=1, at=c(0.55, 1.5, 2.45, 3.4, 4.35, 5.35,
                       6.3, 7.3, 8.3, 9.2), las = 2, decades)
    
    if(add.legend){
      # Plot legend inside the graph or in the outer margin
      if(leg.inside){
        legend("topright", legend = scenarios,fill = c(colors, "#CCCCCC"), bty="n")
        legend("topright", legend = c(rep("", 3),"Observation average", "1990-2019 average"),
               pch=c(NA,NA,NA,20,NA), lty=c(NA,NA,NA,NA,2), bty="n")
      } else{
        legend("topleft", legend="1990-2019 average", lty=2, bty="n", cex=0.8)
        add_legend("topright", legend = c(scenarios, "Observation average"),
                   pch = c(15, 15, 15, 20), pt.cex = c(2, 2, 2, 1),
                   col = c(colors, "#CCCCCC", "black"),
                   bty='n', cex=0.8, ncol=2, text.width=c(0.55, 0.55, 0.5, 0.5))
        
        # pch = c(20, 15, NA, NA, 15, 15), lty=c(NA, NA, 1, 1, NA, NA),
        # lwd=c(NA, NA, 2, 2, NA, NA), pt.cex = c(1, 2, NA, NA, 2, 2),
        # col = c("black", trans_colors[5], colors[1:2], trans_colors[3:4]),
        # bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53)
      }
    }
    if(save.plot){
    dev.off()
    }
  }
  
  return(exptval)
}
##########################################################################
# Precipitation Percent Change Bar Graph
##########################################################################
calcPrecipPercentChangeBarGraph <- function(fips, dataTab, outDir, var, season = NULL, ylabel, 
                                     create.plot=TRUE, leg.inside = TRUE, add.legend=TRUE){
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
  future.decades <- c(2050, 2060, 2070)
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
  
  ## "2050-2079"
  rcp45Tab_ind = which(rcp45Tab$decade %in% future.decades)
  rcp85Tab_ind = which(rcp85Tab$decade %in% future.decades)
  
  rcp45_20502079 = mean(rcp45Tab$wetDays[rcp45Tab_ind])
  rcp85_20502079 = mean(rcp85Tab$wetDays[rcp85Tab_ind])
  
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
  
  # What is the change between vals in 2050-2070 vs 30-year hindcast mean
  val.hind.increase45 = rcp45Tab$wetDays[rcp45Tab_ind] - baseline
  val.hind.increase85 = rcp85Tab$wetDays[rcp85Tab_ind] - baseline
  
  # What is the percent with values above zero?
  likl.hind.increase45 = (length(which(val.hind.increase45 > 0))/length(val.hind.increase45))*100
  likl.hind.increase85 = (length(which(val.hind.increase85 > 0))/length(val.hind.increase85))*100
  
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
                       baseline = baseline,
                       rcp45_20502079 = rcp45_20502079,
                       rcp85_20502079 = rcp85_20502079,
                       hindcast2070incease45 = rcp45_20502079 - baseline, 
                       hindcast2070incease85 = rcp85_20502079 - baseline, 
                       percenthindincrease45 = ((rcp45_20502079 - baseline)/baseline)*100, 
                       percenthindincrease = ((rcp85_20502079 - baseline)/baseline)*100,
                       likl.hind.increase45 = likl.hind.increase45,
                       likl.hind.increase85 = likl.hind.increase85)
  # exptval = data.frame(chngercp45 = round_any(graphing.data['rcp45','2070']*100, 5, floor),
  #                      chngercp85 = round_any(graphing.data['rcp85','2070']*100, 5, floor),
  #                      realrcp45 = round(graphing.data['rcp45','2070']*baseline),
  #                      realrcp85 = round(graphing.data['rcp85','2070']*baseline))
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    # colors = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
    colors = c("#9BC3D7", "#FFB0C3")
    decades <- paste0(colnames(graphing.data), " ")
    scenarios <- c("Low emissions average", "High emissions average", "Hindcast average")
    
    countynme <- unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1))
    ##Plotting
    if(is.null(season)){
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-precipPercentchangeBargraph1.eps"), 
               width=5.72, height=4.04)
    } else {
      cairo_ps(paste0(outDir, countynme, 
                      "-", fips, "-", var, "-", season, "-precipPercentchangeBargraph1.eps"), 
               width=5.72, height=4.04)
    }
    if(!leg.inside){
      par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
    } else {
      par(mgp=c(1.5,.5,0), mar=c(3, 5, 0.5, 1), las=1)
    }
    
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, -1)
    # maxVal <- round(max(graphing.data[1, ], graphing.data[2, ], na.rm=T)*100, 0)
    # minVal <- round(min(graphing.data[3, ], na.rm=T)*100, 1)
    allvals = round((graphing.data[1:3, ])*100, 1)
    
    ##make the bar plot
    #Might need to adjust y axis here and below, as needed
    pretty_axis = pretty(c(min(allvals, na.rm=TRUE), max(allvals, na.rm=TRUE)))
    barplot(graphing.data[1:2, ]*100, beside = T, col = colors, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    par(new = T)
    barplot(graphing.data[3, ]*100, ylim = range(pretty_axis),
            col = "#CCCCCC", width = c(rep(0.8, times = 7)), 
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
    
    if(add.legend){
      # Plot legend inside the graph or in the outer margin
      if(leg.inside){
        legend("topleft", legend = scenarios,
               fill = c(colors, "#CCCCCC"), bty="n")
      } else{
        add_legend("topright", legend = scenarios,
                   fill = c(colors, "#CCCCCC"),
                   bty='n', cex=0.8, ncol=3, text.width=c(0.5, 0.48, 0.43))
      }
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
                                  save.plot = TRUE, leg.inside = FALSE, add.legend = TRUE){
  #################
  # fips <- "shp"
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
  
  if(var == "99tot" | var == "99thdays"){
    stind = 8
  } else {
    stind = 7
  }
  
  #annual means
  statMeans45 <- lapply(decadal45Tabs, function(st){colMeans(st[,stind:ncol(st)])})
  statMeans85 <- lapply(decadal85Tabs, function(st){colMeans(st[,stind:ncol(st)])})
  
  #column mins
  statMins45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(stind:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMins85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(stind:ncol(st),function(col){min(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  #column maxes 
  statMaxs45 <- lapply(decadal45Tabs, function(st){calcVec<-sapply(stind:ncol(st),function(col){max(st[,col])});
  names(calcVec)<-names(statMeans45[[1]]);
  return(calcVec)})
  
  statMaxs85 <- lapply(decadal85Tabs, function(st){calcVec<-sapply(stind:ncol(st),function(col){max(st[,col])});
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
  
  # Extract the values between 2050 and 2070
  fut.wetDays45 <- county45Tabs$wetDays[which(county45Tabs$decade %in% c(2050, 2060, 2070))]
  fut.wetDays85 <- county85Tabs$wetDays[which(county85Tabs$decade %in% c(2050, 2060, 2070))]
  
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
  
  hindave_5yr = unlist(lapply(X=1:length(tsData$rcp45.means), function(X){mean(c(tsData$rcp45.means[X], tsData$rcp85.means[X]))}))
  
  pltYr2019 <- which(tsYrData$year==2019)
  pltYr2070 <- which(tsYrData$year==2079)
  
  # MACA historical is from 1950-2005. Projections are after 2005. Must take the mean of both
  histPoltly <- data.frame(year = tsYrData$year[1:pltYr2019],
                           means = ((tsYrData[1:pltYr2019,]$rcp45.means + tsYrData[1:pltYr2019,]$rcp85.means)/2),
                           mins = ((tsYrData[1:pltYr2019,]$rcp45.mins + tsYrData[1:pltYr2019,]$rcp85.mins)/2),
                           maxs = ((tsYrData[1:pltYr2019,]$rcp45.maxs + tsYrData[1:pltYr2019,]$rcp85.maxs)/2))
  predPoltly <- tsYrData[pltYr2019:pltYr2070,]
  
  # Convert mm to inches
  if(var == "tot" | var == "99tot"){
    predPoltly[ ,2:ncol(predPoltly)] = predPoltly[ ,2:ncol(predPoltly)]/25.4
    histPoltly[ ,2:ncol(histPoltly)] = histPoltly[ ,2:ncol(histPoltly)]/25.4
    tsData[ ,2:ncol(tsData)] = tsData[ ,2:ncol(tsData)]/25.4
    county.obs[ ,obs.ind] = county.obs[ ,obs.ind]/25.4
    rcp45 = rcp45/25.4
    rcp85 = rcp85/25.4
    baseline=baseline/25.4
    fut.wetDays45 <- fut.wetDays45/25.4
    fut.wetDays85 <- fut.wetDays85/25.4
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
  tsr2050 = match(2050, tsData$year)
  tsr2079 = match(2079, tsData$year)
  
  rcp45_20502079 = mean(tsData$rcp45.means[tsr2050:tsr2079])
  rcp85_20502079 = mean(tsData$rcp85.means[tsr2050:tsr2079])
  
  # What is the change between vals in 2050-2070 vs 30-year observation mean
  val.increase45 = fut.wetDays45 - obsmean
  val.increase85 = fut.wetDays85 - obsmean
  
  # What is the percent with values above zero?
  likl.increase45 = (length(which(val.increase45 > 0))/length(val.increase45))*100
  likl.increase85 = (length(which(val.increase85 > 0))/length(val.increase85))*100
  
  # # What is the change between vals in 2050-2070 vs 30-year observation mean
  # val.increase45 = rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")] - obsmean
  # val.increase85 = rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")] - obsmean
  # 
  # # What is the percent with values above zero?
  # likl.increase45 = (length(which(val.increase45 > 0))/length(val.increase45))*100
  # likl.increase85 = (length(which(val.increase85 > 0))/length(val.increase85))*100

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
                       percenttrendincrease45 = (hindcastincrease45/baseline)*100,
                       percenttrendincrease85 = (hindcastincrease85/baseline)*100,
                       # rlt1960 = dayincrease,
                       # trend = round(trend, 2),
                       mean452070 = tsData$rcp45.means[tsr2070],
                       mean852070 = tsData$rcp85.means[tsr2070],
                       rcp45_20502079 = rcp45_20502079,
                       rcp85_20502079 = rcp85_20502079,
                       obs2070incease45 = rcp45_20502079 - obsmean, 
                       obs2070incease85 = rcp85_20502079 - obsmean, 
                       percentobsincrease45 = ((rcp45_20502079 - obsmean)/obsmean)*100, 
                       percentobsincrease85 = ((rcp85_20502079 - obsmean)/obsmean)*100,
                       likl.increase45 = likl.increase45,
                       likl.increase85 = likl.increase85)
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
    if(save.plot){
    if(is.null(season)){
      cairo_ps(paste0(outDir, unique(sapply(strsplit(county45Tabs$county,"_"),"[[",1)), 
                      "-", fips, "-", var,"-precipgraph1Proj.eps"), width=5.72, height=4.04)
    } else{
      cairo_ps(paste0(outDir, unique(sapply(strsplit(county45Tabs$county,"_"),"[[",1)), 
                      "-", fips, "-", var,"-", season, "-precipgraph1Proj.eps"), width=5.72, height=4.04)
    }
    }
    if(add.legend){
      par(mgp=c(1.5,.5,0), mar=c(3, 4, 2.5, 1))
      #par(mar=c(3, 5, 2.5, 1))
    } else {
      par(mgp=c(1.5,.5,0), mar=c(3, 4, 0.5, 1), las=1)
    }
    
    
    plot(0, type="n",xlab="", ylab=ylabel, xaxs="i", yaxt="n", xaxt="n",
         ylim=c(ymintick,ymaxtick), xlim=c(1978.25,2079))
    
    polygon(y = c(predPoltly$rcp85.mins, rev(predPoltly$rcp85.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[4], border = NA)
    polygon(y = c(predPoltly$rcp45.mins, rev(predPoltly$rcp45.maxs)), x = c(predPoltly$year, rev(predPoltly$year)), col = trans_colors[3], border = NA)
    
    polygon(y = c(histPoltly$mins, rev(histPoltly$maxs)), 
            x = c(histPoltly$year, rev(histPoltly$year)), 
            col = trans_colors[5], border = NA)
    
    yrind = match(2019, tsData$year)
    
    lines(tsData$year[1:yrind], hindave_5yr[1:yrind], col="gray60", lwd=2)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp45.means[yrind:nrow(tsData)], col=colors[1], lwd=2)
    lines(tsData$year[yrind:nrow(tsData)], tsData$rcp85.means[yrind:nrow(tsData)], col=colors[2], lwd=2)
    
    points(county.obs$year, county.obs[ ,obs.ind], pch=20, col = "black")
    abline(h=obsmean, lty=2)
    axis(2, las=2)
    axis(1, at=seq(1990,2070, by=20))
    
    # Constructing Legend -----------------------------------------------------
    # scenarios <- c("Observations", "Hindcast range", "Low emissions average", 
    #                "High emissions average", "Low emissions range", 
    #                "High emissions range")
    scenarios <- c("Hindcast average", "Hindcast range", "Low emissions average", 
                   "High emissions average", "Low emissions range", 
                   "High emissions range")
    if(add.legend){
    # Plot legend inside the graph or in the outer margin
    if(leg.inside){
      legend("topleft", legend = scenarios,
             pch = c(NA, NA, 15, 15, 15, 20), lty=c(2, 2, NA, NA, NA, NA),
             lwd=c(2, 2, NA, NA, NA, NA), pt.cex = c(NA, NA, 2, 2, 2, 1),
             col = c(colors[1:2], trans_colors[3:5], "black"), bty="n")
    } else{
      # For Average
      # legend("topleft", legend="1990-2019 average", lty=2, bty="n", cex=0.8)
      # add_legend("topright", legend = scenarios,
      #            pch = c(20, 15, NA, NA, 15, 15), lty=c(NA, NA, 1, 1, NA, NA),
      #            lwd=c(NA, NA, 2, 2, NA, NA), pt.cex = c(1, 2, NA, NA, 2, 2),
      #            col = c("black", trans_colors[5], colors[1:2], trans_colors[3:4]),
      #            bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))#c(0.47, 0.47, 0.43, 0.43, 0.35, 0.35))
      # 
      legend("topleft", legend=c("1990-2019 average", "Observations"), lty=c(2,NA),
             pch=c(NA, 20), bty="n", cex=0.8)
      add_legend("topright", legend = scenarios,
                 pch = c(NA, 15, NA, NA, 15, 15), lty=c(1, NA, 1, 1, NA, NA),
                 lwd=c(2, NA, 2, 2, NA, NA), pt.cex = c(NA, 2, NA, NA, 2, 2),
                 col = c("gray60", trans_colors[5], colors[1:2], trans_colors[3:4]),
                 bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))#c(0.47, 0.47, 0.43, 0.43, 0.35, 0.35))
      
    }
    }
    if(save.plot){
    dev.off()
    }
  }
  return(exptval)
}

##########################################################################
# Precipitation Threshold Graph
##########################################################################
calcPrecipThres <- function(fips, dataTab, obsTab, outDir, var, 
                             ylabel, plot_type = "poly", create.plot=TRUE, leg.inside = TRUE, 
                             add.legend = TRUE, save.plot=TRUE){
  #################
  # fips <- "shp"
  # dataTab <- NHwet99thThres
  # obsTab = NHwet99thObsThres
  # ylabel = "1% precipitation event"
  # # percent=FALSE
  # outDir=outDir
  # # season = NULL
  # var = "99th"
  # plot_type = "box"
  # create.plot = TRUE
  # leg.inside = FALSE
  #################
  
  # Set average periods -----------------------------------------------------
  clim.decades <- c("1990-2019", "2020-2049", "2050-2079")
  
  # Format observations -----------------------------------------------------
  county.obs <- obsTab[obsTab$FIPS==fips,]
  
  # Convert from mm to inches
  county.obs$inches <- county.obs$thres/25.4
  
  # Format hindcasts and projections ----------------------------------------
  ## Seperate the scenarios
  rcp45Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp45",]
  rcp85Tab <- dataTab[dataTab$FIPS==fips & dataTab$rcp=="rcp85",]
 
  # Convert from mm to inches
  rcp45Tab$inches <- rcp45Tab$thres/25.4
  rcp85Tab$inches <- rcp85Tab$thres/25.4
  
  # Calculate the mean, min, and max over the 20 models
  rcp45Means <- sapply(clim.decades, function(dec_yr){mean(rcp45Tab$inches[which(rcp45Tab$period %in% dec_yr)])})
  rcp85Means <- sapply(clim.decades, function(dec_yr){mean(rcp85Tab$inches[which(rcp85Tab$period %in% dec_yr)])})
  rcp45Min <- sapply(clim.decades, function(dec_yr){min(rcp45Tab$inches[which(rcp45Tab$period %in% dec_yr)])})
  rcp85Min <- sapply(clim.decades, function(dec_yr){min(rcp85Tab$inches[which(rcp85Tab$period %in% dec_yr)])})
  rcp45Max <- sapply(clim.decades, function(dec_yr){max(rcp45Tab$inches[which(rcp45Tab$period %in% dec_yr)])})
  rcp85Max <- sapply(clim.decades, function(dec_yr){max(rcp85Tab$inches[which(rcp85Tab$period %in% dec_yr)])})
  rcp45Medians <- sapply(clim.decades, function(dec_yr){median(rcp45Tab$inches[which(rcp45Tab$period %in% dec_yr)])})
  rcp85Medians <- sapply(clim.decades, function(dec_yr){median(rcp85Tab$inches[which(rcp85Tab$period %in% dec_yr)])})

  rcp45df <- data.frame(min = rcp45Min, mean = rcp45Means, max = rcp45Max)
  rcp85df <- data.frame(min = rcp85Min, mean = rcp85Means, max = rcp85Max)
  
  # Export values ----------------------------------------------------------- 
  baseline = mean(c(rcp45df$mean[1], rcp85df$mean[1]))
  hindcastincrease45 = rcp45df$mean[3] - baseline
  hindcastincrease85 = rcp85df$mean[3] - baseline
  obs2070incease45 = (rcp45df$mean[3] - county.obs$inches)
  obs2070incease85 = (rcp85df$mean[3] - county.obs$inches)
  
  medobs2070incease45 = rcp45Medians[3] - county.obs$inches
  medobs2070incease85 = rcp85Medians[3] - county.obs$inches
  
  # What is the change between vals in 2050-2070 vs 30-year observation mean
  val.increase45 = rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")] - county.obs$inches
  val.increase85 = rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")] - county.obs$inches
  val.hind.increase45 = rcp45Tab$inches[which(rcp45Tab$period == "2050-2079")] - baseline
  val.hind.increase85 = rcp85Tab$inches[which(rcp85Tab$period == "2050-2079")] - baseline
  
  # What is the percent with values above zero?
  likl.increase45 = (length(which(val.increase45 > 0))/length(val.increase45))*100
  likl.increase85 = (length(which(val.increase85 > 0))/length(val.increase85))*100
  likl.hind.increase45 = (length(which(val.hind.increase45 > 0))/length(val.hind.increase45))*100
  likl.hind.increase85 = (length(which(val.hind.increase85 > 0))/length(val.hind.increase85))*100
  
  exptval = data.frame(obsmean = county.obs$inches,
                       hindcastmean = baseline,
                       obs2070incease45 = obs2070incease45,
                       obs2070incease85 = obs2070incease85,
                       percentobsincrease45 = (obs2070incease45/county.obs$inches)*100,
                       percentobsincrease85 = (obs2070incease85/county.obs$inches)*100,
                       hindcast2070increase45 = hindcastincrease45,
                       hindcast2070increase85 = hindcastincrease85,
                       percenthindincrease45 = (hindcastincrease45/baseline)*100,
                       percenthindincrease85 = (hindcastincrease85/baseline)*100,
                       rcp45_20502079 = rcp45df$mean[3],
                       rcp85_20502079 = rcp85df$mean[3],
                       likl.increase45 = likl.increase45, 
                       likl.increase85 = likl.increase85,
                       likl.hind.increase45 = likl.hind.increase45, 
                       likl.hind.increase85 = likl.hind.increase85,
                       medrcp45_20502079 = rcp45Medians[3],
                       medrcp85_20502079 = rcp85Medians[3],
                       medobs2070incease45 = medobs2070incease45,
                       medobs2070incease85 = medobs2070incease85,
                       medpercentobsincrease45 = (medobs2070incease45/county.obs$inches)*100,
                       medpercentobsincrease85 = (medobs2070incease85/county.obs$inches)*100)
  
  # Plot --------------------------------------------------------------------
  if(create.plot){
    ##Setting some graphical information
    colors <- c(rgb(0, 0.4, 0.6, 1), rgb(1, 0.2, 0.4, 1), rgb(0, 0.4, 0.6, 0.4), 
                rgb(1, 0.2, 0.4, 0.4), rgb(0.8, 0.8, 0.8, 0.8), "#999999")
    
    # Transparent colors with transparent color function
    trans_colors100 = makeTransparent(colors, 100)
    trans_colors150 = makeTransparent(colors, 150)
    trans_colors = c(trans_colors100[1:4], trans_colors150[5], trans_colors100[6])
    
    colorsbar = c("#9BC3D7", "#FFB0C3")
    
    allvals = c(county.obs$inches, rcp45Tab$inches, rcp85Tab$inches)
    
    # Constructing graph ------------------------------------------------------
    if(plot_type == "vio"){
      if(save.plot){
      cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                      "-", fips, "-", var, "-viograph1.eps"), 
               width=5.72, height=4.04)
      }
      par(mar=c(3, 4, 2, 1), las=1)
      
      
      vioplot(inches~period, data=rcp45Tab, col = c(trans_colors[5], rep(trans_colors[3], 2)),
              plotCentre = "line", side="left", xlab="", ylab=ylabel, xaxt="n", 
              ylim=c(min(allvals, na.rm=TRUE), max(allvals, na.rm=TRUE)))
      
      vioplot(inches~period, data=rcp85Tab, col = c(trans_colors[5], rep(trans_colors[4], 2)), 
              plotCentre = "line", side = "right", add = T)
      
      stripchart(county.obs$inches~county.obs$period, vertical=TRUE, 
                 method = "jitter", pch = 20, add = TRUE, col="black")
      
      axis(side=1, at=1:nrow(rcp45df), las = 1, label=rownames(rcp45df), tick=FALSE)
      
      points(2:3, rcp45df$mean[2:3], pch = 19, col = colors[1])
      points(2:3, rcp85df$mean[2:3], pch = 19, col = colors[2])
      
      # Constructing Legend -----------------------------------------------------
      scenarios <- c("Hindcast range", "Observations", 
                     "Low emissions average", "High emissions average",
                     "Low emissions range", "High emissions range")
      
      # Plot legend inside the graph or in the outer margin
      if(add.legend){
        if(leg.inside){
          legend("topleft", legend = scenarios,
                 pch = c(19, 19, 15, 15, 15, 20), pt.cex = c(1, 1, 2, 2, 2, 1),
                 col = c(colors[1:2], trans_colors[3:5], "black"), bty="n")
        } else{
          # For Average
          add_legend("topright", legend = scenarios,
                     pch = c(15, 20, 19, 19, 15, 15), pt.cex = c(2, 1, 1, 1, 2, 2),
                     col = c(trans_colors[5], "black", colors[1:2], trans_colors[3:4]),
                     bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))
        }
      }
      if(save.plot){
      dev.off()
      }
    } else if(plot_type == "percbar"){
      if(save.plot){
    cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                    "-", fips, "-", var, "-percbargraph1.eps"), 
             width=5.72, height=4.04)
      }
    if(leg.inside){
      par(mgp=c(1.5,.5,0), mar=c(3, 5, 0.5, 1), las=1)
    } else{
      par(mar=c(3, 4, 2, 1), las=1)
    }
      
    
    graphing.data = mat.or.vec(3,3)
    graphing.data[1,] = c((baseline - baseline), rep(NA, 2))
    graphing.data[2,] = c(NA, (rcp45df$mean[2:3] - baseline))
    graphing.data[3,] = c(NA, (rcp85df$mean[2:3] - baseline))
    graphing.data = (graphing.data/baseline)*100
    
    #colors_bar = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
    colors_bar = colorsbar
    ##make the bar plot
    #Might need to adjust y axis here and below, as needed
    pretty_axis = pretty(range(graphing.data, na.rm=TRUE))
    barplot(graphing.data[2:3, ], beside = T, col = colors_bar, 
            ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
            space = c(0, 0.4))
    box()
    par(new = T)
    barplot(graphing.data[1, ], ylim = range(pretty_axis),
            col = "#CCCCCC", width = c(rep(0.8, times = 7)), 
            xlab = "", xaxt = "n", axes = F)
    abline(h = 0, lty = 3, lwd = 1, col = "black")
    # abline(linreg45)
    # abline(linreg85)
    # box()
    
    #Adding x and y axes text
    axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, "%"))
    mtext(side=2, line=2.7, at=mean(pretty_axis), ylabel, font=1, las=0)
    # mtext(side=1, at=c(0.55, 1.5, 2.45), las = 1, clim.decades)
    axis(side=1, at=c(0.55, 1.5, 2.45), las = 1, label=rownames(rcp45df), tick=FALSE)
    
    scenarios <- c("Low emissions average", "High emissions average", "Hindcast average")
    
    # Plot legend inside the graph or in the outer margin
    if(leg.inside){
      legend("topleft", legend = scenarios,
             fill = c(colors_bar, "#CCCCCC"), bty="n")
    } else{
      add_legend("topright", legend = scenarios,
                 fill = c(colors_bar, "#CCCCCC"),
                 bty='n', cex=0.8, ncol=3, text.width=c(0.45, 0.43, 0.35))
    }
    if(save.plot){
    dev.off()
    }
    } else if(plot_type == "bar"){
      if(save.plot){
      cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                      "-", fips, "-", var, "-obsbargraph1.eps"), 
               width=5.72, height=4.04)
      }
      if(leg.inside){
        # par(mgp=c(1.5,.5,0), mar=c(3, 5, 0.5, 1), las=1)
        par(mgp=c(1.5,.5,0), mar=c(2, 4.5, 0.5, 3), las=1)
      } else{
        par(mar=c(3, 4, 2, 1), las=1)
      }
      
      
      graphing.data = mat.or.vec(3,3)
      graphing.data[1,] = c(baseline, rep(NA, 2))
      graphing.data[2,] = c(NA, rcp45df$mean[2:3])
      graphing.data[3,] = c(NA, rcp85df$mean[2:3])
      # graphing.data = (graphing.data/baseline)*100
      
      #colors_bar = c(rgb(0, 0.4, 0.6, 0.6), rgb(1, 0.2, 0.4, 0.6))
      colors_bar = colorsbar
      
      ##make the bar plot
      #Might need to adjust y axis here and below, as needed
      pretty_axis = pretty(range(c(graphing.data, county.obs$inches), na.rm=TRUE))
      barplot(graphing.data[2:3, ], beside = T, col = colors_bar, 
              ylim = range(pretty_axis), xlab = "", xaxt = "n", axes = F, 
              space = c(0, 0.4), xpd=FALSE)
      par(new = T)
      barplot(graphing.data[1, ], ylim = range(pretty_axis),
              col = "#CCCCCC", width = c(rep(0.8, times = 7)), 
              xlab = "", xaxt = "n", axes = F, xpd=FALSE)
      # abline(h = 0, lty = 3, lwd = 1, col = "black")
      # abline(linreg45)
      # abline(linreg85)
      box()
      points(0.55, county.obs$inches, pch=20)
      
      # Find percent change at roughly the same axis values
      # perchange = round(((pretty_axis - county.obs$inches)/county.obs$inches)*100)
      perchange = seq(-9, 18, by=3)
      atchange = ((perchange/100)*county.obs$inches)+county.obs$inches
      
      #Adding x and y axes text
      axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, " "))
      axis(side = 4, at = atchange, labels = paste0(" ", perchange, "%"))
      mtext(side=2, line=2.5, at=mean(pretty_axis), ylabel, font=1, las=0)
      mtext(side=4, line=1.9, at=mean(pretty_axis), "Percent change from observed 1990-2019", 
            font=1, las=0)
      # mtext(side=1, at=c(0.55, 1.5, 2.45), las = 1, clim.decades)
      axis(side=1, at=c(0.55, 1.5, 2.45), las = 1, label=rownames(rcp45df), tick=FALSE)
      
      scenarios <- c("Low emissions average", "High emissions average", 
                     "Hindcast average", "Observation average")
      
      # Plot legend inside the graph or in the outer margin
      if(leg.inside){
        legend("topleft", legend = scenarios, pch=c(rep(22,3), 20),
               pt.bg = c(colors_bar, "#CCCCCC", NA), col="black", 
               pt.cex = c(2,2,2,1), bty="n")
      } else{
        add_legend("topright", legend = scenarios,
                   fill = c(colors_bar, "#CCCCCC"),
                   bty='n', cex=0.8, ncol=3, text.width=c(0.45, 0.43, 0.35))
      }
      if(save.plot){
      dev.off()
      }
    } else if(plot_type == "box"){
      
      ##Assembling into a data frame for plotting
      hindAve = rep(NA, nrow(rcp45Tab))
      for(i in 1:nrow(rcp45Tab)){
        hindAve[i] = mean(rcp45Tab$inches[i], rcp85Tab$inches[i])
      }
      hindTab = data.frame(period = rcp45Tab$period, model = rcp45Tab$model, inches=hindAve)
      subhind = hindTab[which(hindTab$period %in% unique(hindTab$period)[1]), ]
      subrcp45 = rcp45Tab[which(rcp45Tab$period %in% unique(hindTab$period)[2:3]), ]
      subrcp85 = rcp85Tab[which(rcp85Tab$period %in% unique(hindTab$period)[2:3]), ]
      
      datatest = data.frame(period = c(subhind$period, subrcp45$period, subrcp85$period), 
                            inches = c(subhind$inches, subrcp45$inches, subrcp85$inches),
                            type = c(rep("hindcast", nrow(subhind)), 
                                     rep("rcp45", nrow(subrcp45)),
                                     rep("rcp85", nrow(subrcp85))))
      
      df2 = datatest
      df2$type <- factor(df2$type, levels = c("hindcast","rcp45","rcp85"))
      df2$period = as.character(df2$period)
      
      # Count the number of decades (for boxplot)
      decadeind = which(clim.decades %in% unique(county.obs$period))
      for(dind in decadeind){
        county.obs$int[which(county.obs$period == clim.decades[dind])] <- dind
      }
      county.obs$shape = "Observation average" # for legend
      
      # compute lower and upper whiskers limits
      ylim1 = boxplot.stats(df2$inches)$stats[c(1, 5)]
      
      ylim2 = c(min(c(ylim1[1], county.obs$inches)), 
                max(c(county.obs$inches, ylim1[2])))
        
        transf = function(x){
          ((x-county.obs$inches)/county.obs$inches)
        }
        
        #Our transformation function
        scaleFUN <- function(x){x=x*100; sprintf("%f", x); paste0(x, "%")}
      #legend.margin=margin(0,0,0,-6), legend.spacing.x = unit(0.1, "cm"),
      if(add.legend){
        bpl <- ggplot(data = df2, aes(x=period,y=inches, color=type)) +
          stat_boxplot(geom = "errorbar")+
          scale_color_manual(values=rep("black", 3))+
          geom_boxplot(aes(fill=type), outlier.shape = NA) +
          theme_bw() + 
          theme_update(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank()) +
          theme(legend.position="top", legend.box.spacing = unit(0, "pt"),
                legend.margin=margin(0,0,0,-5), legend.spacing.x = unit(0.1, "cm"),
                legend.text=element_text(size=9),
                axis.title = element_text(size = 12, colour="black"), 
                axis.text = element_text(size = 12, colour="black"),
                axis.title.y.right = element_text(angle = 90)) +
          geom_point(data=county.obs, aes(x=int, y=inches, shape = shape), color="black") +
          labs(x="", y=ylabel, color="", fill="", shape="") + 
          scale_y_continuous(
            # Add a second axis and specify its features
            sec.axis = sec_axis(transf,
                                name="Percent change from observed 1990-2019",
                                breaks = seq(-.09,.18,.03),
                                labels=scaleFUN)) +
          # scale_y_continuous(
          #   # Add a second axis and specify its features
          #   sec.axis = sec_axis(name="Percent change from observed 1990-2019 (%)", 
          #                       breaks = atchange, labels = labchange)) +
          guides(color="none") + 
          scale_fill_manual(values=c("#CCCCCC", "#9BC3D7", "#FFB0C3"), 
                            labels=c('Hindcast', 'Low emissions range', 'High emissions range')) +
          coord_cartesian(ylim = ylim2)
      } else {
        bpl <- ggplot(data = df2, aes(x=period,y=inches, color=type)) +
          stat_boxplot(geom = "errorbar")+
          scale_color_manual(values=rep("black", 3))+
          geom_boxplot(aes(fill=type), outlier.shape = NA, show.legend = FALSE) +
          theme_bw() + 
          theme_update(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank()) +
          theme(axis.title = element_text(size = 14, colour="black"), 
                axis.text = element_text(size = 14, colour="black")) +
          geom_point(data=county.obs, aes(x=int, y=inches, shape = shape), 
                     color="black", show.legend = FALSE) +
          labs(x="", y=ylabel, color="", fill="", shape="") + 
          guides(color="none") +
          scale_fill_manual(values=c("#CCCCCC", "#9BC3D7", "#FFB0C3")) +
          coord_cartesian(ylim = ylim2)
        # print(bp)
      }
        if(save.plot){
      psname = paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)),
                      "-", fips, "-", var, "-Boxgraph1.eps")
      ggsave(filename=psname, plot=bpl, width=5.72, height=4.04, units="in")
        }
    
  } else if(plot_type == "poly"){
    if(save.plot){
    cairo_ps(paste0(outDir, unique(sapply(strsplit(rcp45Tab$county,"_"),"[[",1)), 
                    "-", fips, "-", var, "-percpolygraph1.eps"), 
             width=5.72, height=4.04)
    }
    # par(mar=c(3, 5, 2.5, 1), las=1)
    par(mgp=c(1.8,.5,0), mar=c(2, 4, 2.5, 3.75), las=1)
    # par(mar=c(2.5, 4.5, 2.5, 1), las=1)
    #par(mar=c(3, 4, 2, 1), las=1)
    
    plot(0, type="n", xlab="", ylab=ylabel, xaxt="n", 
         ylim=c(min(allvals, na.rm=TRUE), max(allvals, na.rm=TRUE)), 
         xlim=c(0.5,3.5))
    
    rect(0.5, mean(c(rcp45df$min[1], rcp85df$min[1])), 1.5, 
         mean(c(rcp45df$max[1], rcp85df$max[1])), col=trans_colors[5], border=NA)
    rect(1.5, rcp85df$min[2], 2.5, rcp85df$max[2], col=trans_colors[4], border=NA)
    rect(2.5, rcp85df$min[3], 3.5, rcp85df$max[3], col=trans_colors[4], border=NA)
    
    rect(1.5, rcp45df$min[2], 2.5, rcp45df$max[2], col=trans_colors[3], border=NA)
    rect(2.5, rcp45df$min[3], 3.5, rcp45df$max[3], col=trans_colors[3], border=NA)
    
    points(1, county.obs$inches, pch = 20, col="black")
    lines(c(0.5, 1.5), rep(baseline, 2), col="gray60", lwd=3)
    lines(c(1.5, 2.5), rep(rcp85df$mean[2], 2), col=colors[2], lwd=3)
    lines(c(2.5, 3.5), rep(rcp85df$mean[3], 2), col=colors[2], lwd=3)
    lines(c(1.5, 2.5), rep(rcp45df$mean[2], 2), col=colors[1], lwd=3)
    lines(c(2.5, 3.5), rep(rcp45df$mean[3], 2), col=colors[1], lwd=3)
    
    # mtext(side=1, at=1:3, las = 1, clim.decades)
    axis(side=1, at=1:3, las = 1, label=rownames(rcp45df), tick=FALSE)
    
    # Find percent change at roughly the same axis values
    # perchange = round(((axTicks(2) - county.obs$inches)/county.obs$inches)*100)
    # if(!any(perchange == 0)){perchange = c(perchange, 0)}
    perchange = seq(-9, 18, by=3)
    atchange = ((perchange/100)*county.obs$inches)+county.obs$inches
    
    #Adding x and y axes text
    # axis(side = 2, at = pretty_axis, labels = paste0(pretty_axis, " "))
    axis(side = 4, at = atchange, labels = paste0(" ", perchange, "%"))
    # mtext(side=2, line=2.5, at=mean(pretty_axis), ylabel, font=1, las=0)
    mtext(side=4, line=2.5, at=mean(axTicks(2)), "Percent change from observed 1990-2019", 
          font=1, las=0)
    
    # Constructing Legend -----------------------------------------------------
    # scenarios <- c("Hindcast range", "Observation average", 
    #                "Low emissions average", "High emissions average",
    #                "Low emissions range", "High emissions range")
    scenarios <- c("Hindcast range", "Hindcast average", 
                   "Low emissions average", "High emissions average",
                   "Low emissions range", "High emissions range")
    
    # Plot legend inside the graph or in the outer margin
    if(add.legend){
      if(leg.inside){
        legend("topleft", legend = scenarios,
               pch = c(19, 19, 15, 15, 15, 20), pt.cex = c(1, 1, 2, 2, 2, 1),
               col = c(colors[1:2], trans_colors[3:5], "black"), bty="n")

      } else{
        # For Average
        # add_legend("topright", legend = scenarios,
        #            pch = c(15, 20, NA, NA, 15, 15), pt.cex = c(2, 1, NA, NA, 2, 2),
        #            lty = c(NA, NA, 1, 1, NA, NA), lwd = c(NA, NA, 2, 2, NA, NA), 
        #            col = c(trans_colors[5], "black", colors[1:2], trans_colors[3:4]),
        #            bty='n', cex=0.8, ncol=3, text.width=c(0.49, 0.49, 0.54, 0.54, 0.53, 0.53))
        # add_legend("topright", legend = scenarios,
        #            pch = c(15, 20, NA, NA, 15, 15), pt.cex = c(2, 1, NA, NA, 2, 2),
        #            lty = c(NA, NA, 1, 1, NA, NA), lwd = c(NA, NA, 2, 2, NA, NA), 
        #            col = c(trans_colors[5], "black", colors[1:2], trans_colors[3:4]),
        #            bty='n', cex=0.8, ncol=3, text.width=c(0.53, 0.53, 0.54, 0.54, 0.53, 0.53))
        legend("topleft", legend = "Observation average", pch = 20, col = "black", cex=0.8, bty="n")
        
        add_legend("topright", legend = scenarios,
                   pch = c(15, NA, NA, NA, 15, 15), pt.cex = c(2, NA, NA, NA, 2, 2),
                   lty = c(NA, 1, 1, 1, NA, NA), lwd = c(NA, 2, 2, 2, NA, NA), 
                   col = c(trans_colors[5], "gray60", colors[1:2], trans_colors[3:4]),
                   bty='n', cex=0.8, ncol=3, text.width=c(0.53, 0.53, 0.54, 0.54, 0.53, 0.53))
      }
    }
    if(save.plot){
    dev.off()
    }
  }
  }
  if(!save.plot & plot_type == "box"){
    return(bpl)
  } else {
    return(exptval)
  }
}

##########################################################################
# Likelihood Definitions
# https://www.ipcc.ch/site/assets/uploads/2018/05/uncertainty-guidance-note.pdf 
# Virtually certain (99-100% probability)
# Very likely (90-100% probability)
# Likely (66-100% probability)
# About as likely as not (33 to 66% probability)
# Unlikely (0-33% probability)
# Very unlikely (0-10% probability)
# Exceptionally unlikely (0-1% probability)
##########################################################################
define.likl = function(likl){
  if(likl >= 99){
    def = 'is virtually certain to'
  } else if(likl >= 90 & likl < 99){
    def = 'will very likely'
  } else if(likl >= 66 & likl < 90) {
    def = 'will likely'
  } else if(likl >= 33 & likl < 66) {
    def = 'is about as likely as not to'
  } else if(likl > 10 & likl < 33) {
    def = 'is unlikely to'
  } else if(likl > 1 & likl <= 10) {
    def = 'is very unlikely to'
  } else if(likl >= 0 & likl <= 1) {
    def = 'is exceptionally unlikely to'
  } else {
    stop("Check likl value")
  }
  return(def)
}

single.likl = function(likl, var.word){
  val = define.likl(likl)
  pval = round(likl)
  output = paste0(val, " ", var.word, " (", pval, "% chance)")
  return(output)
}

double.likl = function(likl.45, likl.85, var.word){
  val.45 = define.likl(likl.45)
  val.85 = define.likl(likl.85)
  p45 = round(likl.45)
  p85 = round(likl.85)
  if(val.45 == val.85){
    if(p45 == p85){
      output = paste0(val.45, " ", var.word, " (", p45, "% chance of increasing)")
    } else {
      output = paste0(val.45, " ", var.word, " (between a ", p45, " and ", p85, "% chance of increasing)")
    }
  } else {
    output = paste0(val.45, ", to ", val.85, " ", var.word, " (between a ", 
                    p45, " and ", p85, "% chance of increasing)")
  }
  return(output)
}

##########################################################################
# Change Definitions
# -5% ≥ percent: Decreasing
# -5% < percent < 5%: Little to no change
# 5% ≤ percent: Increasing
##########################################################################

define.change = function(percent){
  if(percent >= 5){
    def = "increase by"
  } else if(percent > -5 & percent < 5){
    def = "have little to no change"
  } else if(percent <= -5) {
    def = "decrease by"
  } else {
    stop("Check percent value")
  }
  return(def)
}

single.change = function(percent, amount, units){
  pval = round(percent)
  val = define.change(pval)
  amt = round(amount, 1)
  
  if(val == "have little to no change"){
    output = paste0("will ", val, "remaining at roughly", amt, " ", units)
  } else {
    output = paste0("will ", val, " ", pval, "% to ", amt, " ", units)
  }
  return(output)
}

double.change = function(percent.45, percent.85, amount.45, amount.85, units){
  p45 = round(percent.45)
  p85 = round(percent.85)
  val.45 = define.change(p45)
  val.85 = define.change(p85)

  if(val.45 == val.85){
    if(val.45 == "have little to no change"){
      output = paste0("will ", val.45)
     } else {
        if(p45 == p85){
          output = paste0("will ", val.45, " ", p45, "%")
         } else {
          output = paste0("will ", val.45, " ", p45, " and ", p85, "%")
         }
     }
      } else {
      output = paste0("will ", val.45, ", to ", val.85, " ", 
                    p45, " and ", p85, "%")
    }
  
  amt45 = round(amount.45, 1)
  amt85 = round(amount.85, 1)
  
  if(amt45 == amt85){
    amount = paste(amt45, units)
  } else {
    amount = paste(amt45, "and", amt85, units)
  }
  
  if(val.45 == "have little to no change"){
    output = paste(output, "remaining at roughly", amount)
   } else {
      output = paste(output, "to", amount)
    }
    
  return(output)
}

##########################################################################
# END
##########################################################################

