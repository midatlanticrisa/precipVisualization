##########################################################################
##########################################################################
exAndCalcPR <- function(nc, decs, prThres){
  #################
  #nc <- prFiles[3]
  #decs <- centDecs
  #prThres <- 0.1  ##mm/day
  #################
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,".1950"),"[[",1),"grid."), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
  rcp <- substr(rcpMod, 2, 6)
  model <- substr(rcpMod, 8, nchar(rcpMod))
  
  ##read in the data from the file
  rastBrick <- brick(nc)
  brickNames <- names(rastBrick)
  isoYrs <- substr(brickNames, 2,  nchar(brickNames[1]))
  minYr <- min(as.numeric(substr(isoYrs, 1,  4)))
  maxYr <- max(as.numeric(substr(isoYrs, 1,  4)))
  rastPts <- rasterToPoints(rastBrick[[1]], spatial=T)
  
  ##cycle through the given year pairs and preform the calcuations
  #cds <- decs[1]
  subByDec <- lapply(decs, function(cds){stYrInd<-which(isoYrs==paste0(max(cds-10,minYr),"0101"));
                                            edYrInd<-which(isoYrs==paste0(min(cds+19,maxYr),"1231"));
                                            subStack<-rastBrick[[stYrInd:edYrInd]];
                                            exRast<-extract(subStack, rastPts, sp=F);
                                            exRast[exRast<prThres] <- NA;
                                            return(exRast)})
  
  ##calculate the percentile in each cell, and then average
  #dat<-subByDec[[1]]
  calcMeanPerc <- sapply(subByDec, function(dat){calcPerc<-apply(dat,1,quantile,probs=0.99,na.rm=T);
                                                  avePerc<-mean(calcPerc);
                                                  return(avePerc)})
  ##calculate the percentile for all of the data
  calcPerc <- sapply(subByDec, quantile, probs=0.99,na.rm=T)
  ##set up the output
  outFrame <- data.frame(county=cntyName, FIPS=cntyFips, rcp=rcp, model=model, centralDecade=decs, meanPercentile=calcMeanPerc, percentile=calcPerc)
  
  return(outFrame)
}

##########################################################################
# select.blocks.gt.length() Function from PCICt and climdex.pcic
##########################################################################
select.blocks.gt.length <- function (d, n, na.value = FALSE) {
  stopifnot(is.logical(d), is.numeric(n))
  if (n < 1) 
    return(d)
  if (n >= length(d)) 
    return(rep(FALSE, length(d)))
  d[is.na(d)] <- na.value
  d2 <- Reduce(function(x, y) {
    return(c(rep(FALSE, y), d[1:(length(d) - y)]) & x)
  }, 1:n, d)
  return(Reduce(function(x, y) {
    return(c(d2[(y + 1):length(d2)], rep(FALSE, y)) | x)
  }, 1:n, d2))
}

##########################################################################
# Function to read the netcdf files and format into 30 year matrices
##########################################################################
read.nc <- function(nc, type, decs, inRCP, trueDate){
  # Start the clock!
  # ptm <- proc.time()
  
  ##read in the data from the file
  fn <- nc_open(nc)
  # Extract the variable
  if(type=="temp"){
    var <- ncvar_get(fn,"air_temperature")
  } else if(type=="pr"){
    if(inRCP){
      var <- ncvar_get(fn,"precipitation") # MACA
    } else {
      var <- ncvar_get(fn,"precipitation_amount") # GridMET
    }
  }
  
  # lonw <- ncvar_get(fn,"longitude")
  # latw <- ncvar_get(fn,"latitude")
  
  # Extrac the time variable
  if(trueDate==F){ # GridMET
    days <- ncvar_get(fn,"day")
    # Format dates from days since 1900-01-01 to real dates as a simple string
    tw = gsub("-", "", as.character(as.Date(days, origin="1900/01/01")))
  } else { # MACA
    tw <- ncvar_get(fn,"time")
  }
  nc_close(fn)
  length(tw)
  # dim(latw); dim(lonw) 
  minYr <- min(as.numeric(substr(tw, 1,  4))) # first year
  maxYr <- max(as.numeric(substr(tw, 1,  4))) # last year
  
  # Convert the data into a matrix of
  # location by time
  val = mat.or.vec((dim(var)[1]*dim(var)[2]), dim(var)[3])
  for(i in 1:dim(var)[3]){
    val[ ,i] = as.vector(var[, ,i]) 
  }
  # Determine which locations have no values
  val_true = rep(NA, nrow(val))
  for(j in 1:nrow(val)){
    val_true[j] = all(is.na(val[j, ])) 
  }
  if(all(val_true)){
    # if there are no grid points then return NA
    return(NA)
  } else {
    # Keep only the locations with values
    val = as.data.frame(val[!val_true, ])
    # If there is only 1 grid point then transform the matrix so time is columns
    if(dim(val)[2] == 1){ 
      val = t(val)
    }
    colnames(val) = paste0("X", tw)
    # Stop the clock
    # proc.time() - ptm
    
    ##cycle through the given year pairs and preform the calcuations
    # For each time period interval (30 years) subset the daily values at each grid point
    subByDec <- lapply(decs, function(cds){stYrInd<-which(tw==paste0(max(cds-12,minYr),"0101"));
    edYrInd<-which(tw==paste0(min(cds+21,maxYr),"1231"));
    if(length(edYrInd)==0){
      edYrInd<-which(tw==paste0(min(cds+21,maxYr),"0830"));
    }
    subStack<-val[ ,stYrInd:edYrInd];
    return(subStack)})
    
    return(subByDec)
  }
}
##########################################################################
# Function to read the netcdf files and format into 50 year matrices
##########################################################################
read.nc.50 <- function(nc, type, decs, inRCP, trueDate){
  # Start the clock!
  # ptm <- proc.time()
  
  ##read in the data from the file
  fn <- nc_open(nc)
  # Extract the variable
  if(type=="temp"){
    var <- ncvar_get(fn,"air_temperature")
  } else if(type=="pr"){
    if(inRCP){
      var <- ncvar_get(fn,"precipitation") # MACA
    } else {
      var <- ncvar_get(fn,"precipitation_amount") # GridMET
    }
  }
  
  # lonw <- ncvar_get(fn,"longitude")
  # latw <- ncvar_get(fn,"latitude")
  
  # Extrac the time variable
  if(trueDate==F){ # GridMET
    days <- ncvar_get(fn,"day")
    # Format dates from days since 1900-01-01 to real dates as a simple string
    tw = gsub("-", "", as.character(as.Date(days, origin="1900/01/01")))
  } else { # MACA
    tw <- ncvar_get(fn,"time")
  }
  nc_close(fn)
  length(tw)
  # dim(latw); dim(lonw) 
  minYr <- min(as.numeric(substr(tw, 1,  4))) # first year
  maxYr <- max(as.numeric(substr(tw, 1,  4))) # last year
  
  # Convert the data into a matrix of
  # location by time
  val = mat.or.vec((dim(var)[1]*dim(var)[2]), dim(var)[3])
  for(i in 1:dim(var)[3]){
    val[ ,i] = as.vector(var[, ,i]) 
  }
  # Determine which locations have no values
  val_true = rep(NA, nrow(val))
  for(j in 1:nrow(val)){
    val_true[j] = all(is.na(val[j, ])) 
  }
  if(all(val_true)){
    # if there are no grid points then return NA
    return(NA)
  } else {
    # Keep only the locations with values
    val = as.data.frame(val[!val_true, ])
    # If there is only 1 grid point then transform the matrix so time is columns
    if(dim(val)[2] == 1){ 
      val = t(val)
    }
    colnames(val) = paste0("X", tw)
    # Stop the clock
    # proc.time() - ptm
    
    ##cycle through the given year pairs and preform the calcuations
    # For each time period interval (30 years) subset the daily values at each grid point
    subByDec <- lapply(decs, function(cds){stYrInd<-which(tw==paste0(max(cds,minYr),"0101"));
    edYrInd<-which(tw==paste0(min(cds+49,maxYr),"1231"));
    if(length(edYrInd)==0){
      edYrInd<-which(tw==paste0(min(cds+49,maxYr),"0830"));
    }
    subStack<-val[ ,stYrInd:edYrInd];
    return(subStack)})
    
    return(subByDec)
  }
}

##########################################################################
# Function to read the netcdf files and find the 99th percentile threshold of 
# daily precipitation
##########################################################################
read.nc.99th <- function(nc, type, inRCP, trueDate, startDate="19900101", endDate="20191231"){
  # Start the clock!
  # ptm <- proc.time()
  
  ##read in the data from the file
  fn <- nc_open(nc)
  # Extract the variable
  if(type=="temp"){
    var <- ncvar_get(fn,"air_temperature")
  } else if(type=="pr"){
    if(inRCP){
      var <- ncvar_get(fn,"precipitation") # MACA
    } else {
      var <- ncvar_get(fn,"precipitation_amount") # GridMET
    }
  }
  
  # lonw <- ncvar_get(fn,"longitude")
  # latw <- ncvar_get(fn,"latitude")
  
  # Extrac the time variable
  if(trueDate==F){ # GridMET
    days <- ncvar_get(fn,"day")
    # Format dates from days since 1900-01-01 to real dates as a simple string
    tw = gsub("-", "", as.character(as.Date(days, origin="1900/01/01")))
  } else { # MACA
    tw <- ncvar_get(fn,"time")
  }
  nc_close(fn)
  length(tw)
  # dim(latw); dim(lonw) 
  minYr <- min(as.numeric(substr(tw, 1,  4))) # first year
  maxYr <- max(as.numeric(substr(tw, 1,  4))) # last year
  
  # Convert the data into a matrix of
  # location by time
  val = mat.or.vec((dim(var)[1]*dim(var)[2]), dim(var)[3])
  for(i in 1:dim(var)[3]){
    val[ ,i] = as.vector(var[, ,i]) 
  }
  # Determine which locations have no values
  val_true = rep(NA, nrow(val))
  for(j in 1:nrow(val)){
    val_true[j] = all(is.na(val[j, ])) 
  }
  if(all(val_true)){
    # if there are no grid points then return NA
    return(NA)
  } else {
    # Keep only the locations with values
    val = as.data.frame(val[!val_true, ])
    # If there is only 1 grid point then transform the matrix so time is columns
    if(dim(val)[2] == 1){ 
      val = t(val)
    }
    colnames(val) = paste0("X", tw)
    # Stop the clock
    # proc.time() - ptm
    
    ##cycle through the given year pairs and preform the calcuations
    # For each time period interval (30 years) subset the daily values at each grid point
    # subByDec <- lapply(decs, function(cds){stYrInd<-which(tw==paste0(max(cds,minYr),"0101"));
    # edYrInd<-which(tw==paste0(min(cds+49,maxYr),"1231"));
    # if(length(edYrInd)==0){
    #   edYrInd<-which(tw==paste0(min(cds+49,maxYr),"0830"));
    # }
    stYrInd<-which(tw==startDate)
    edYrInd<-which(tw==endDate)
    subStack<-val[ ,stYrInd:edYrInd]
    # return(subStack)})
    
    datNames<-colnames(subStack);
    if(is.null(datNames)){datNames<-names(subStack)}; # if only one grid cell
    decYrs<-unique(as.numeric(substr(datNames,2,5)));
    ##isolate and summarize data by year
    yrSumm<-sapply(decYrs, function(yr){
      if(is.null(dim(subStack))){ # if only one grid cell
        yrData<-subStack[grep(paste0("X",yr),datNames)];
        per99 <- quantile(yrData[-which(yrData<0.5)], prob=0.99)
      } else {
        yrData<-as.matrix(subStack[,grep(paste0("X",yr),datNames)]);
        per99 <- apply(yrData, 1, function(X){quantile(X[-which(X<0.5)], prob=0.99)})
      }
      return(per99)})
    per99mean <- mean(yrSumm)
    
    # # For each station, the 99th percentile threshold of daily precipitation was determined 
    # # from the 1901-2016 data using only days with at least 0.5 mm of precipitation.
    # if(is.null(dim(subStack))){ # if one grid point
    #   per99 <- quantile(subStack[-which(subStack<0.5)], prob=0.99)
    # } else {
    #   per99grids <- apply(subStack, 1, function(X){quantile(X[-which(X<0.5)], prob=0.99)})
    #   per99 <- mean(per99grids)
    # }
    return(per99mean)
  }
}

##########################################################################
# Number of days in a year above a threshold
##########################################################################
exceedThres <- function(data, thres){
  data[which(data<thres)]<-0 # days less than the threshold aren't counted
  data[which(data!=0)]<-1
  if(is.null(dim(data))){ # if one grid point
    numDays<-sum(data) # Calculate the number of days in a year
  } else {
    yearSums<-rowSums(data) # Calculate the number of days in a year
    numDays<-mean(yearSums) # average the results across all grid points
  }
  return(numDays)
}
##########################################################################
# Total for a period of time
##########################################################################
calcTotal <- function(data){
  if(is.null(dim(data))){ # if one grid point
    numDays<-sum(data) # Sum the precip for the year
  } else {
    yearSums<-rowSums(data) # Sum the precip for the year at each location
    numDays<-mean(yearSums) # average the results across all grid points
  }
  return(numDays)
}

##########################################################################
# Decluster time series
##########################################################################
decluster_timeseries <- function(dataset, max.length, min.dt, sub.mult = 2) {
  decluster <- list()
  dec <- list()
  
  decluster$time = as.Date(gsub("X", "", names(dataset)), format="%Y%m%d")
  if(is.null(dim(dataset))){
    decluster$vals = as.numeric(dataset)
  } else {
    decluster$vals = as.numeric(dataset[1,])
  }
  ordind = order(decluster$vals, decreasing = TRUE)
  
  decluster = as.data.frame(decluster)
  decluster = decluster[ordind, ]
  decluster$rank = 1:nrow(decluster)
  
  top = decluster[1:(max.length*sub.mult), ]
  
  timeind = order(top$time)
  top = top[timeind, ]
  
  tdiff <- diff(top$time)
  ind.too.close <- which(tdiff <= min.dt)
  if(length(ind.too.close) > 0) {
    # go through the places where there are time.series values too close together,
    # and set to throw the smaller value away.
    # need to account for the possibility that there are more than two values in
    # a 'cluster'.
    # indices where new clusters begin: (tack on first one manually)
    ind.clusters <- c(ind.too.close[1] , ind.too.close[which(diff(ind.too.close) > 1) + 1])
    if(length(ind.clusters) > 1) {
      # case where there are multiple clusters to consider
      # initialize by fixing to remove all of the spots that are too close. then
      # we will remove the indices of each cluster's maximum
      irem <- unique(c(ind.too.close, ind.too.close + 1))
      for (i in 1:length(ind.clusters)) {
        # how long is this cluster?
        if(i < length(ind.clusters)) {
          first <- ind.clusters[i]
          last  <- ind.too.close[which(ind.too.close==ind.clusters[i+1]) -1]+1
        } else {
          first <- ind.clusters[i]
          if (length(which(ind.too.close > first))==0) {last <- first + 1
          } else {last <- first+length(which(ind.too.close > first)) + 1}
        }
        ind.this.cluster <- first:last
        isave <- ind.this.cluster[which.max(top$vals[first:last])]
        # remove this cluster's maximum from irem; they might be out of order
        isave <- which(irem==isave)
        irem <- irem[-isave]
      }
    } else {
      # case with only one cluster
      # initialize by fixing to remove all of the cluster. then we will remove
      # the index of the cluster maximum from this
      irem <- unique(c(ind.too.close, ind.too.close + 1))
      irem <- irem[-which.max(top$vals[irem])]
    }
    dec$time <- top$time[-irem]
    dec$rank <- top$rank[-irem]
    dec$vals <- top$vals[-irem]
  } else {
    dec$time <- top$time
    dec$rank <- top$rank
    dec$vals <- top$vals
  }
  dec = as.data.frame(dec)
  if(nrow(dec) < max.length){
   # stop(paste0("Declustered timeseries is less than ", max.length, 
   #             ". Increase the sub.mult to sample a larger subset timeseries."))
  dec = "FALSE"
  } else {
  ind = which(dec$rank %in% 1:max.length)
  dec = dec[ind, ]
  # dec = dec[order(dec$vals, decreasing = TRUE), ]
  }
  return(dec)
}

##########################################################################
# Calculate the IDF change factor for each gridcell in a county
##########################################################################
calcChangeFactor <- function(nc, decs, type="pr", fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  print(cntyName)
  
  subByDec <- read.nc.50(nc, type, decs, inRCP, trueDate)
  
  probs = c(50, 20, 10, 4, 2, 1)/100
  return.period = 1/probs
  
  if((length(subByDec) && is.na(subByDec))){
    # if there are no grid points (i.e., NA, then do nothing)
    print(paste(cntyName, "has no grid points."))
  } else {
    gev_curves <- lapply(subByDec, function(dat){
      if(is.null(dim(dat))){ # if only one grid cell
        pds = decluster_timeseries(dat, max.length = 50, min.dt = 7, sub.mult = 2);
   
        if(pds == FALSE){
          pds = decluster_timeseries(dat, max.length = 50, min.dt = 7, sub.mult = 3);
        }
        if(pds == FALSE){
          pds = decluster_timeseries(dat, max.length = 50, min.dt = 7, sub.mult = 4);
        }
        if(pds == FALSE){
          stop(paste0("Declustered timeseries is less than ", max.length, 
                      "even after sub.mult=4. Increase the sub.mult to sample a larger subset timeseries."))
        }

        lmoments = samlmu(pds$vals);
        gevparams = pelgev(lmoments);
        quant = quagev(1-probs, para = gevparams);
        idf = as.data.frame(t(quant)); #make sure 1 row dataframe
        
      } else {
        idf = lapply(X=1:nrow(dat), function(X){pds = decluster_timeseries(dat[X, ], 
                                                                           max.length = 50, 
                                                                           min.dt = 7, 
                                                                           sub.mult = 2);
        if(pds == FALSE){
          pds = decluster_timeseries(dat[X, ], max.length = 50, min.dt = 7, sub.mult = 3);
        }
        if(pds == FALSE){
          pds = decluster_timeseries(dat[X, ], max.length = 50, min.dt = 7, sub.mult = 4);
        }
        if(pds == FALSE){
          stop(paste0("Declustered timeseries is less than ", max.length, 
                      "even after sub.mult=4. Increase the sub.mult to sample a larger subset timeseries."))
        }

        lmoments = samlmu(pds$vals);
        gevparams = pelgev(lmoments);
        
        quant = quagev(1-probs, para = gevparams);
        names(quant) = paste0(return.period, "yr");
        return(quant)});
        
        idf = do.call(rbind.data.frame, idf);
      }

      colnames(idf) = paste0(return.period, "yr");
      return(idf)})
    
    change_factors = gev_curves[[2]]/gev_curves[[1]]
    pastfut = c(rep(paste0(decs[1], "-", decs[1]+49), nrow(gev_curves[[1]])), 
                rep(paste0(decs[2], "-", decs[2]+49), nrow(gev_curves[[2]])))
    fullgev = cbind.data.frame(years = pastfut, rbind(gev_curves[[1]], gev_curves[[2]]))
    
    changeFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, 
                                               model=model), change_factors)
    gevFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, 
                                            model=model), fullgev)
    return(list(changeFrame, gevFrame))
  }
}

##########################################################################
# Calculate rainfall indexes seasonally
##########################################################################
calcSeasonCycle <- function(nc, decs, thres, type, fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  #################
  #nc <- prFiles[1]
  #nc <- tMaxFiles[3]
  #decs <- centDecs
  #thres <- conv_unit(2.0,"inch","mm")
  #thres <- conv_unit(95, "F", "K")
  #type <- "extdays"
  # fileNMSplt1 <- ".1950"
  # fileNMSplt2 <- "grid."
  # inRCP=T
  # trueDate=T
  #################
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  print(cntyName)
  ##read in the data from the file
  # Extract the variable
  subByDec <- read.nc.50(nc, "pr", decs, inRCP, trueDate)
  if((length(subByDec) && is.na(subByDec))){
    # if there are no grid points (i.e., NA, then do nothing)
    print(paste(cntyName, "has no grid points."))
  } else {
    ##the number of hot or cold (frost) days per year, and then average over a few time periods
    #dat<-subByDec[[1]]
    # dat = subByDec[[1]]
    calcHotDays <- lapply(subByDec, function(dat){datNames<-colnames(dat);
    if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
    decYrs<-unique(as.numeric(substr(datNames,2,5)));
    ##isolate and summarize data by year
    # yr = decYrs[1]
    yrSumm<-sapply(decYrs, function(yr){
      
      if(is.null(dim(dat))){ # If one grid cell
        yrData<-dat[grep(paste0("X",yr),names(dat))];
        
        # Extract dates as R dates for that year
        dates = as.Date(gsub("X", "", names(yrData)), format="%Y%m%d");
        
        # Extract just the months
        month.series <- as.numeric(strftime(dates,"%m")); # get.months(dates)
        
        ts.month = lapply(X=1:12, function(X){yrData[which(month.series == X)]});
       
      } else { # If multiple grid cells
        yrData<-as.matrix(dat[,grep(paste0("X",yr),colnames(dat))]);

        # Extract dates as R dates for that year
        dates = as.Date(gsub("X", "", colnames(yrData)), format="%Y%m%d");
        
        # Extract just the months
        month.series <- as.numeric(strftime(dates,"%m")); # get.months(dates)
        
        ts.month = lapply(X=1:12, function(X){yrData[ ,which(month.series == X)]});
      }
      
      # Calculate extreme days or total
      if(type=="extdays"){
        days.month <- as.data.frame(t(unlist(lapply(ts.month, exceedThres, thres=thres))));
      } else {
        days.month <- as.data.frame(t(unlist(lapply(ts.month, calcTotal))));
      }
      colnames(days.month) <- c("jan", "feb", "mar", "apr", "may", "jun",
                                "jul", "aug", "sep", "oct", "nov", "dec");
      return(days.month)})
    
    colnames(yrSumm) <- decYrs;
    return(yrSumm)})

    
    # ##set up the output
      outFrame <- lapply(X=1:length(decs), function(X){cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, start50yrs=decs[X]), 
                                                                        months=rownames(calcHotDays[[X]]), calcHotDays[[X]])})
    
    return(outFrame)
  }
}
##########################################################################
# Number of days in a year above or below a threshold
##########################################################################
calc99thThreshold <- function(nc, decsStart=1990, decsEnd=2019, type="pr", 
                              fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  #################
  #nc <- prFiles[1]
  #nc <- tMaxFiles[3]
  #decs <- centDecs
  #thres <- conv_unit(2.0,"inch","mm")
  #thres <- conv_unit(95, "F", "K")
  #type <- "pr"
  #fileNMSplt1 <- ".1950"
  #fileNMSplt2 <- "grid."
  #inRCP=T
  #trueDate=T
  #################
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  print(cntyName)
  
  # Calculate the thresholds
  thres <- lapply(X=1:length(decsStart), function(X){read.nc.99th(nc, type, inRCP, trueDate, 
                                                                  startDate=paste0(decsStart[X], "0101"), 
                                                                  endDate=paste0(decsEnd[X], "1231"))})
  ##set up the output
  if(inRCP==T){
    outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model), period=paste(decsStart, decsEnd, sep="-"), thres=unlist(thres))
  }else{
    outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, period=paste(decsStart, decsEnd, sep="-"), thres=unlist(thres)))
  }
  return(outFrame)
}
##########################################################################
# Number of days in a year above or below a threshold
##########################################################################
calcThresNumDays <- function(nc, decs, thres, type, fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  #################
  #nc <- prFiles[1]
  #nc <- tMaxFiles[3]
  #decs <- centDecs
  #thres <- conv_unit(2.0,"inch","mm")
  #thres <- conv_unit(95, "F", "K")
  #type <- "pr"
  #fileNMSplt1 <- ".1950"
  #fileNMSplt2 <- "grid."
  #inRCP=T
  #trueDate=T
  #################
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  print(cntyName)
  
  ##read in the data from the file
  # Extract the variable
  if(type=="temp"){
    subByDec <- read.nc(nc, "temp", decs, inRCP, trueDate)
  } else if(type=="pr" | type=="99thpr"){
    subByDec <- read.nc(nc, "pr", decs, inRCP, trueDate)
  } else if(type=="frost"){
    subByDec <- read.nc(nc, "temp", decs, inRCP, trueDate)
  }
  
  if((length(subByDec) && is.na(subByDec))){
    # if there are no grid points (i.e., NA, then do nothing)
    print(paste(cntyName, "has no grid points."))
  } else {
    
    ##the number of hot or cold (frost) days per year, and then average over a few time periods
    #dat<-subByDec[[1]]
    if(type=="frost"){
      calcHotDays <- lapply(subByDec, function(dat){datNames<-colnames(dat);
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5)));
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          yrData[which(yrData>thres)]<-0; # days greater than the threshold aren't counted
          yrData[which(yrData!=0)]<-1;
          numDays<-sum(yrData); # Calculate the number of days in a year
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          yrData[which(yrData>thres)]<-0; # days greater than the threshold aren't counted
          yrData[which(yrData!=0)]<-1;
          yearSums<-rowSums(yrData); # Calculate the number of days in a year
          numDays<-mean(yearSums); # average the results across all grid points
        }
        return(numDays)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      ##calculate the five year average time series
      timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
      return(list(thirYrAve, timSeries5, yrSumm))})
      
    } else if(type=="99thpr"){
      ##the number of hot days per year, and then average over a few time periods
      #dat<-subByDec[[1]]
      if(thres == "99th"){
        thres <- read.nc.99th(nc, "pr", inRCP, trueDate) # Determine the 99th threshold for 1990-2019 period
      }
      calcHotDays <- lapply(subByDec, function(dat){datNames<-colnames(dat); # extract all dates
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5))); # extract years
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          yrData[which(yrData<thres)]<-0; # days less than the threshold aren't counted
          yrData[which(yrData!=0)]<-1; # any day above threshold is one day
          numDays<-sum(yrData); # Calculate the number of days in a year
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          yrData[which(yrData<thres)]<-0; # days less than the threshold aren't counted
          yrData[which(yrData!=0)]<-1;
          yearSums<-rowSums(yrData); # Sum the days for the year at each location
          numDays<-mean(yearSums); # average the results across all grid points
        }
        return(numDays)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      ##calculate the five year average time series
      timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
      return(list(thirYrAve, timSeries5, yrSumm))})
    } else {
      calcHotDays <- lapply(subByDec, function(dat){datNames<-colnames(dat);
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5)));
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          yrData[which(yrData<thres)]<-0; # days less than the threshold aren't counted
          yrData[which(yrData!=0)]<-1;
          numDays<-sum(yrData); # Calculate the number of days in a year
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          yrData[which(yrData<thres)]<-0; # days less than the threshold aren't counted
          yrData[which(yrData!=0)]<-1;
          yearSums<-rowSums(yrData); # Calculate the number of days in a year
          numDays<-mean(yearSums); # average the results across all grid points
        }
        return(numDays)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      ##calculate the five year average time series
      timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
      return(list(thirYrAve, timSeries5, yrSumm))})
    }
    
    ##set up the output
    thirYrAve <- sapply(calcHotDays, function(chd){chd[[1]]})
    timSeries5 <- lapply(calcHotDays, function(chd){chd[[2]]})
    yearSumms <- lapply(calcHotDays, function(chd){chd[[3]]})
    yrVect <- do.call(c, yearSumms)
    uniNames <- unique(names(yrVect))
    uniYrs <- sapply(uniNames, function(yr){yrVect[which(names(yrVect)==yr)[1]]})
    names(uniYrs) <- uniNames
    timSeries5 <- do.call(rbind.data.frame, timSeries5)
    
    if(type=="temp"){
      colnames(timSeries5) <- paste0("fiveYrAveHotDaysCentYr_", -10:19)
      if(inRCP==T){
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveHotDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), hotDays=uniYrs))
      }else{
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveHotDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), hotDays=uniYrs))
      }
    }else if(type=="pr"){
      colnames(timSeries5) <- paste0("fiveYrAveWetDaysCentYr_", -10:19)
      if(inRCP==T){
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }else{
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }
    } else if(type=="frost"){
      colnames(timSeries5) <- paste0("fiveYrAveFrostDaysCentYr_", -10:19)
      if(inRCP==T){
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveFrostDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), frostDays=uniYrs))
      }else{
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveFrostDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), frostDays=uniYrs))
      }
    } else if(type=="99thpr"){
      if(inRCP==T){
        colnames(timSeries5) <- paste0("fiveYrAveWetDaysCentYr_", -10:19)
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve, threshold=thres), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }else{
        colnames(timSeries5) <- paste0("fiveYrAveTot_vmm",gsub("\\.", "p", round(thres, 2)),"WetDaysCentYr_", -10:19)
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve, threshold=thres), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }
    }
    
    return(list(outFrame, yrFrame))
  }
}
##########################################################################
# Total precipitation 
##########################################################################
calcThresWetTot <- function(nc, decs, thres, base, type, fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  #################
  #nc <- prFiles[1]
  #nc <- tMaxFiles[3]
  #decs <- centDecs
  # thres=0.95
  # base = "1960-1989"
  #thres <- conv_unit(2.0,"inch","mm")
  #thres <- conv_unit(95, "F", "K")
  #type <- "tot"
  #fileNMSplt1 <- ".1950"
  #fileNMSplt2 <- "grid."
  #inRCP=T
  #trueDate=T
  ################# 
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  
  ##read in the data from the file
  # Extract the variable
  subByDec <- read.nc(nc, "pr", decs, inRCP, trueDate)
  if((length(subByDec) && is.na(subByDec))){
    # if there are no grid points (i.e., NA, then do nothing)
    print(paste(cntyName, "has no grid points."))
  } else {
    
    if(type=="tot_thres95"){
      # Calculate 95%-ile threshold for the base period
      calc95 <- lapply(subByDec, function(dat){datNames<-colnames(dat); # extract all dates
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5))); # extract years
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          mean95 <- quantile(yrData, thres); # extract the 95% 
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          # extract the 95% at each location
          ann95 <- apply(yrData, 1, function(X){quantile(X, thres)}); 
          # Average across all locations
          mean95<-mean(ann95);
        }
        return(mean95)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      return(c(thirYrAve, paste(decYrs[3], decYrs[32], sep="-")))})
      thresholds = do.call(rbind.data.frame, calc95)
      colnames(thresholds) <- c("threshold_mm", "baseperiod")
      baseind = which(thresholds$baseperiod == base)
      thresval = thresholds$threshold_mm[baseind]
      
      # Use the calculated threshold to determine the number of days exceeding
      calctot <- lapply(subByDec, function(dat){datNames<-colnames(dat); # extract all dates
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5))); # extract years
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          yrData[which(yrData<thresval)]<-0; # set values below the threshold to 0
          numDays<-sum(yrData); # Sum the precip for the year at each location
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          yrData[which(yrData<thresval)]<-0; # set values below the threshold to 0
          yearSums<-rowSums(yrData); # Sum the precip for the year at each location
          numDays<-mean(yearSums); # average the results across all grid points
        }
        return(numDays)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      ##calculate the five year average time series
      timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
      return(list(thirYrAve, timSeries5, yrSumm))})
      
    } else if(type=="tot"){
      ##the number of hot days per year, and then average over a few time periods
      #dat<-subByDec[[1]]
      calctot <- lapply(subByDec, function(dat){datNames<-colnames(dat); # extract all dates
      if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
      decYrs<-unique(as.numeric(substr(datNames,2,5))); # extract years
      ##isolate and summarize data by year
      yrSumm<-sapply(decYrs, function(yr){
        if(is.null(dim(dat))){ # if only one grid cell
          yrData<-dat[grep(paste0("X",yr),datNames)];
          numDays<-sum(yrData); # Sum the precip for the year at each location
        } else {
          yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
          yearSums<-rowSums(yrData); # Sum the precip for the year at each location
          numDays<-mean(yearSums); # average the results across all grid points
        }
        return(numDays)})
      names(yrSumm) <- decYrs
      ##calculate the thirty year average for the decade
      thirYrAve<-mean(yrSumm[3:32], na.rm=T);
      ##calculate the five year average time series
      timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
      return(list(thirYrAve, timSeries5, yrSumm))})
      
      } else if(type=="tot_thres"){
        ##the number of hot days per year, and then average over a few time periods
        #dat<-subByDec[[1]]
        if(thres == "99th"){
          thres <- read.nc.99th(nc, "pr", inRCP, trueDate)
        }
          
        calctot <- lapply(subByDec, function(dat){datNames<-colnames(dat); # extract all dates
        if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
        decYrs<-unique(as.numeric(substr(datNames,2,5))); # extract years
        ##isolate and summarize data by year
        yrSumm<-sapply(decYrs, function(yr){
          if(is.null(dim(dat))){ # if only one grid cell
            yrData<-dat[grep(paste0("X",yr),datNames)];
            yrData[which(yrData<thres)]<-0; # set values below the threshold to 0
            numDays<-sum(yrData); # Sum the precip for the year at each location
          } else {
            yrData<-as.matrix(dat[,grep(paste0("X",yr),datNames)]);
            yrData[which(yrData<thres)]<-0; # set values below the threshold to 0
            yearSums<-rowSums(yrData); # Sum the precip for the year at each location
            numDays<-mean(yearSums); # average the results across all grid points
          }
          return(numDays)})
        names(yrSumm) <- decYrs
        ##calculate the thirty year average for the decade
        thirYrAve<-mean(yrSumm[3:32], na.rm=T);
        ##calculate the five year average time series
        timSeries5 <- sapply(3:32, function(centYr){mean(yrSumm[(centYr-2):(centYr+2)], na.rm=T)});
        return(list(thirYrAve, timSeries5, yrSumm))})
        } else {
      stop("Type unknown. Must be 'tot', 'tot_thres95', or 'tot_thres'.")
    }
    
    ##set up the output
    thirYrAve <- sapply(calctot, function(chd){chd[[1]]})
    timSeries5 <- lapply(calctot, function(chd){chd[[2]]})
    yearSumms <- lapply(calctot, function(chd){chd[[3]]})
    yrVect <- do.call(c, yearSumms)
    uniNames <- unique(names(yrVect))
    uniYrs <- sapply(uniNames, function(yr){yrVect[which(names(yrVect)==yr)[1]]})
    names(uniYrs) <- uniNames
    timSeries5 <- do.call(rbind.data.frame, timSeries5)
    
    if(type=="tot_thres"){
      #colnames(timSeries5) <- paste0("fiveYrAveTotWetDaysCentYr_", -10:19)
      if(inRCP==T){
       colnames(timSeries5) <- paste0("fiveYrAveTotWetDaysCentYr_", -10:19)
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve, threshold=thres), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }else{
        colnames(timSeries5) <- paste0("fiveYrAveTot_vmm",gsub("\\.", "p", round(thres, 2)),"WetDaysCentYr_", -10:19)
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve, threshold=thres), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }
    }else if(type=="tot_thres95"){
      colnames(timSeries5) <- paste0("fiveYrAveTot",gsub("\\.", "p", thres),"WetDaysCentYr_", -10:19)
      if(inRCP==T){
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve, pct95=thresholds$threshold_mm), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }else{
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve, pct95=thresholds$threshold_mm), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }
    } else if(type=="tot"){
      colnames(timSeries5) <- paste0("fiveYrAveTotWetDaysCentYr_", -10:19)
      if(inRCP==T){
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }else{
        outFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve), timSeries5)
        yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(names(uniYrs)), wetDays=uniYrs))
      }
    }
    
    return(list(outFrame, yrFrame))
  }
}

##########################################################################
# Calculate rainfall indexes seasonally
##########################################################################
calcSeasonExt <- function(nc, decs, seasons, thres, type, fileNMSplt1, fileNMSplt2, inRCP=T, trueDate=T){
  #################
  #nc <- prFiles[1]
  #nc <- tMaxFiles[3]
  #decs <- centDecs
  #thres <- conv_unit(2.0,"inch","mm")
  #thres <- conv_unit(95, "F", "K")
  #type <- "extdays"
  #fileNMSplt1 <- ".1950"
  #fileNMSplt2 <- "grid."
  #inRCP=T
  #trueDate=T
  #################
  ##extract various aspects of the file name
  fileName <- sapply(strsplit(nc, "/"), "[[", length(strsplit(nc, "/")[[1]]))
  cntyName <- sapply(strsplit(sapply(strsplit(fileName,fileNMSplt1),"[[",1),fileNMSplt2), "[[", 2)
  cntyFips <- sapply(strsplit(cntyName, "_"), "[[", 3)
  if(inRCP==T){
    rcpMod <- sapply(strsplit(sapply(strsplit(fileName,".0."),"[[",1),"cal"), "[[", 2)
    wrcp <- substr(rcpMod, 2, 6)
    model <- substr(rcpMod, 8, nchar(rcpMod))
  }
  print(cntyName)
  ##read in the data from the file
  # Extract the variable
  subByDec <- read.nc(nc, "pr", decs, inRCP, trueDate)
  if((length(subByDec) && is.na(subByDec))){
    # if there are no grid points (i.e., NA, then do nothing)
    print(paste(cntyName, "has no grid points."))
  } else {
    ##the number of hot or cold (frost) days per year, and then average over a few time periods
    #dat<-subByDec[[1]]
    # dat = subByDec[[1]]
    calcHotDays <- lapply(subByDec, function(dat){datNames<-colnames(dat);
    if(is.null(datNames)){datNames<-names(dat)}; # if only one grid cell
    decYrs<-unique(as.numeric(substr(datNames,2,5)));
    ##isolate and summarize data by year
    # yr = decYrs[1]
    yrSumm<-sapply(decYrs, function(yr){
      # Isolate the seasons:
      winter = c(12, 1:2)
      spring = 3:5;
      summer = 6:8;
      fall = 9:11;
      
      if(is.null(dim(dat))){ # If one grid cell
        yrData<-dat[grep(paste0("X",yr),names(dat))];
        if(yr == decYrs[[1]]){
          wintyrData<-NA;
        } else {
          wintyrData<-dat[grep(paste0("X",(yr-1)),names(dat))];
        }
        # Extract dates as R dates for that year
        dates = as.Date(gsub("X", "", names(yrData)), format="%Y%m%d");
        wintdates = as.Date(gsub("X", "", names(wintyrData)), format="%Y%m%d");
        
        # Extract just the months
        month.series <- as.numeric(strftime(dates,"%m")); # get.months(dates)
        month.winter <- as.numeric(strftime(wintdates,"%m")); # get.months(dates)
        if(yr == decYrs[[1]]){
          tot.winter <- NA
        } else {
          ts.winter = c(wintyrData[which(month.winter == winter[1])], yrData[c(which(month.series == winter[2]), which(month.series == winter[3]))]);
          # Calculate extreme days or total
          if(type=="extdays"){
            tot.winter <- exceedThres(ts.winter, thres);
          } else {
            tot.winter <- calcTotal(ts.winter);
          }
        }
        ts.spring = yrData[c(which(month.series == spring[1]), which(month.series == spring[2]), which(month.series == spring[3]))];
        ts.summer = yrData[c(which(month.series == summer[1]), which(month.series == summer[2]), which(month.series == summer[3]))];
        ts.fall = yrData[c(which(month.series == fall[1]), which(month.series == fall[2]), which(month.series == fall[3]))];
        
      } else { # If multiple grid cells
        yrData<-as.matrix(dat[,grep(paste0("X",yr),colnames(dat))]);
        if(yr == decYrs[[1]]){
          wintyrData<-NA;
        } else {
          wintyrData<-as.matrix(dat[,grep(paste0("X",(yr-1)),colnames(dat))]);
        }
        # Extract dates as R dates for that year
        dates = as.Date(gsub("X", "", colnames(yrData)), format="%Y%m%d");
        wintdates = as.Date(gsub("X", "", colnames(wintyrData)), format="%Y%m%d");
        
        # Extract just the months
        month.series <- as.numeric(strftime(dates,"%m")); # get.months(dates)
        month.winter <- as.numeric(strftime(wintdates,"%m")); # get.months(dates)
        if(yr == decYrs[[1]]){
          tot.winter <- NA
        } else {
          ts.winter = cbind(wintyrData[ ,which(month.winter == winter[1])], yrData[ ,c(which(month.series == winter[2]), which(month.series == winter[3]))]);
          # Calculate extreme days or total
          if(type=="extdays"){
            tot.winter <- exceedThres(ts.winter, thres);
          } else {
            tot.winter <- calcTotal(ts.winter);
          }
        }
        ts.spring = yrData[ ,c(which(month.series == spring[1]), which(month.series == spring[2]), which(month.series == spring[3]))];
        ts.summer = yrData[ ,c(which(month.series == summer[1]), which(month.series == summer[2]), which(month.series == summer[3]))];
        ts.fall = yrData[ ,c(which(month.series == fall[1]), which(month.series == fall[2]), which(month.series == fall[3]))];
      }
      
      # Calculate extreme days or total
      if(type=="extdays"){
        days.spring <- exceedThres(ts.spring, thres);
        days.summer <- exceedThres(ts.summer, thres);
        days.fall <- exceedThres(ts.fall, thres);
      } else {
        days.spring <- calcTotal(ts.spring);
        days.summer <- calcTotal(ts.summer);
        days.fall <- calcTotal(ts.fall);
      }
      
      numDays<-data.frame(winter = tot.winter, spring = days.spring, summer = days.summer, fall = days.fall);
      
      # yrData[which(yrData<thres)]<-0; # days less than the threshold aren't counted
      # yrData[which(yrData!=0)]<-1;
      # yearSums<-rowSums(yrData); # Calculate the number of days in a year
      # numDays<-mean(yearSums); # average the results across all grid points
      return(numDays)})
    colnames(yrSumm) <- decYrs;
    rownames(yrSumm) <- seasons;
    # Append NAs if less than 32 years
    if(ncol(yrSumm) < 34){
      yrSummNA <- matrix(NA, nrow=length(seasons), ncol = (34-ncol(yrSumm)))
      yrSumm <- cbind(yrSumm, yrSummNA)
    }
    ##calculate the thirty year average for the decade
    thirYrAve<-sapply(X=1:length(seasons), function(X){mean(as.numeric(yrSumm[X,3:32]), na.rm=T)});
    # thirYrAve<-as.data.frame(sapply(X=1:3, function(X){mean(as.numeric(yrSumm[X,3:32]), na.rm=T)}));
    # rownames(thirYrAve) <- seasons;
    ##calculate the five year average time series
    timSeries5 <- lapply(X=1:length(seasons), function(X){sapply(3:32, function(centYr){mean(as.numeric(yrSumm[X, (centYr-2):(centYr+2)]), na.rm=T)})});
    timSeries5 <- do.call(rbind.data.frame, timSeries5);
    colnames(timSeries5) <- decYrs[3:32];
    rownames(timSeries5) <- seasons;
    return(list(thirYrAve, timSeries5, yrSumm))})
    
    # fix the first value in each 30 year period for winter running averages
    # Because the first value starts with NA so the 5 year average is just 4 years.
    # Using the next period we can get all 5 values for the average
    # fix 1960, 1990, 2020, 2050
    timefix <- decs-10
    calcHotDays[[1]][[2]][1,1] <- NA # Set 1960 to NA because we don't have winter values for 1958
    for(ind in 2:length(timefix)){
      centind1 = which(colnames(calcHotDays[[(ind-1)]][[3]]) == as.character(timefix[ind]))
      centind2 = which(colnames(calcHotDays[[ind]][[3]]) == as.character(timefix[ind]))
      fixed = mean(unlist(c(calcHotDays[[(ind-1)]][[3]][1, (centind1-2):centind1], 
                            calcHotDays[[ind]][[3]][1, (centind2+1):(centind2+2)])))
      calcHotDays[[ind]][[2]][1,1] <- fixed
    }
    
    ##set up the output
    thirYrAve <- sapply(calcHotDays, function(chd){chd[[1]]})
    colnames(thirYrAve) <- decs
    rownames(thirYrAve) <- seasons
    timSeries5 <- lapply(calcHotDays, function(chd){chd[[2]]})
    yearSumms <- lapply(calcHotDays, function(chd){chd[[3]]})
    #yrVect <- do.call(c, yearSumms)
    yrVect <- do.call(cbind.data.frame, yearSumms)
    # uniNames <- unique(names(yrVect))
    uniNames <- unique(colnames(yrVect))
    yearsOnly = !grepl("[A-Za-z]+", uniNames)
    uniNames <- uniNames[yearsOnly]
    # uniYrs <- sapply(uniNames, function(yr){yrVect[which(names(yrVect)==yr)[1]]})
    uniYrs <- sapply(uniNames, function(yr){yrVect[ ,which(colnames(yrVect)==yr)[1]]})
    # names(uniYrs) <- uniNames
    # timSeries5 <- do.call(rbind.data.frame, timSeries5)
    
    bindTimSeries5 <- function(seasonind, timSeries5, decs){
      timSeries5seas <- timSeries5[[1]][seasonind,]
      for(X in 2:length(decs)){timSeries5seas <- rbind(timSeries5seas, timSeries5[[X]][seasonind,])}
      return(timSeries5seas)
    }
    for(X in 1:length(decs)){colnames(timSeries5[[X]]) <- paste0("fiveYrAveWetDaysCentYr_", -10:19)}
    timSeries5seasons <- lapply(X=1:length(seasons), bindTimSeries5, timSeries5 = timSeries5, decs = decs)
    
    # if(type=="pr"){
    # colnames(timSeries5seas) <- paste0("fiveYrAveWetDaysCentYr_", -10:19)
    if(inRCP==T){
      outFrame <- lapply(X=1:length(seasons), function(X){cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, centralDecade=decs, aveWetDays=thirYrAve[X,]), timSeries5seasons[[X]])})
      names(outFrame) <- seasons
      yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(colnames(uniYrs)), t(uniYrs)))
      
      # yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, rcp=wrcp, model=model, year=as.numeric(colnames(uniYrs)), wetDays=uniYrs))
    }else{
      outFrame <- lapply(X=1:length(seasons), function(X){cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, centralDecade=decs, aveWetDays=thirYrAve[X,]), timSeries5seasons[[X]])})
      names(outFrame) <- seasons
      yrFrame <- cbind.data.frame(data.frame(county=cntyName, FIPS=cntyFips, year=as.numeric(colnames(uniYrs)), t(uniYrs)))
    }
    # }
    
    return(list(outFrame, yrFrame))
  }
}
##########################################################################
##########################################################################
