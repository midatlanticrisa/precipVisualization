inDir <- "~/Documents/Github/marisa-map-backup/scripts/"
outDir <- "~/Downloads/"
plotDir <- paste0(outDir, "Tide_figs/")


metaDat = tideStations[1, ]
vars = c("air_temperature", "air_pressure", 
                  "visibility", "humidity", "wind", 
                  "water_level", "water_temperature", 
                  "conductivity", "salinity")
units = "english"
tz = "lst"
dates = "latest"
datum=NULL
begin_date=NULL
range=48

varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?date=",dates, 
                 "&station=", metaDat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                 tz, "&units=", units, "&format=xml")

# Download the observations for each variable
obsList <- lapply(varsURL, function(URL){
  station_data = xmlToList(rawToChar(GET(URL)$content))
}

URL = varsURL[1]

station_data = xmlToList(rawToChar(GET(URL)$content))
stationdf = station_data$observations
df = cbind(t(as.data.frame(station_data$metadata)), as.data.frame(stationdf),
           row.names = NULL)

# Download the observations for each variable
obsList <- lapply(varsURL, function(X){
  print(X)
  sd = read_xml(X)
  return(sd)})

station_data2 = read_xml(varsURL[1])
metadf = xml_attrs(xml_children(station_data2))[[1]]
obs = xml_attrs(xml_children(xml_children(station_data2)))

obsname = colnames(xmlToDataFrame(nodes=getNodeSet(station_xml, "//observations")))
obsdf = as.data.frame(obs, col.names = obsname)

cbind(t(as.data.frame(station_data$metadata)), obsdf,
      row.names = NULL)

stationdf = do.call(rbind, station_data2$observations)

# Parse into an R structure representing XML tree
station_xml <- xmlParse(station_data2)

# Convert the parsed XML to a dataframe
tideStation_df <- colnames(xmlToDataFrame(nodes=getNodeSet(station_xml, "//observations")))


collectTideData = function(metaDat, vars = c("air_temperature", "air_pressure", 
                                             "visibility", "humidity", "wind", 
                                             "water_level", "water_temperature", 
                                             "conductivity", "salinity"),
                           units = "english", tz = "lst", dates = "latest",
                           datum=NULL, begin_date=NULL, range=48){
  
  # Datum is mandatory for all water level products to correct the data to the reference point desired.
  is.greatlakes <- noquote(as.character(metaDat$greatlakes))
  if(is.null(datum)){
    if(is.greatlakes){
      datum <- "IGLD" # International Great Lakes Datum.
    } else {
      datum <- "MLLW" # Mean Lower Low Water (Nautical Chart Datum for all U.S. coastal waters)
    }
  }
  
  # Generate a URL for each variable based on function input
  if(length(vars) == 1 && vars == "ofs_water_level"){
    varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=",begin_date, 
                     "&range=", range, "&station=", metaDat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                     tz, "&units=", units, "&format=xml")
  } else {
    varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?date=",dates, 
                     "&station=", metaDat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                     tz, "&units=", units, "&format=xml")
  }
  
# Download the observations for each variable
obsList <- lapply(varsURL, function(URL){
  # URL = varsURL[3]
  #print(URL)
  station_data = read_xml(URL)
  
  metadf = xml_attrs(xml_children(station_data))[[1]]
  
  # If the station doesn't record the variable an error will appear.
  # Report missing variables as NA
  if(any(names(as_list(station_data)$data) == "error")){
      df = as.data.frame(t(as.data.frame(metadf)))
      df$t = NA
      df$v = NA

    } else { # Read the observations.
      obs = xml_attrs(xml_children(xml_children(station_data)))
      
      if(length(obs) > 1){
        obsdf = do.call(rbind, obs)
        # Suppress row.names to stop warnings messages of duplicate row.names
        # Names will be recorded in the list
        df = cbind(t(as.data.frame(metadf)), as.data.frame(obsdf), row.names = NULL)
        
      } else {
        # Parse into an R structure representing XML tree
        station_xml <- xmlParse(station_data)
        
        obsname = colnames(xmlToDataFrame(nodes=getNodeSet(station_xml, "//observations")))
        obsdf = as.data.frame(obs, col.names = obsname)
        
        df = cbind(t(as.data.frame(metadf)), obsdf, row.names = NULL)
      }
    }
  return(df)
})
# Record the variable names
names(obsList) = vars

# Merge the list of data frames filling in missing columns
tide_df <- rbindlist(obsList, fill = TRUE, idcol=TRUE)
colnames(tide_df)[1] = "var" # change from ".id"
tide_df$datum <- datum

return(tide_df)
}

formatTideStations = lapply(X=1:nrow(tideStations), function(X){
  tide_data <- collectTideData(tideStations[X, ])
  tide_format <- parseTideData(tide_data)
  return(tide_format)
})


# Create the tidal plots with operational forecasts where available
lapply(X=1:nrow(tideStations), function(X){
  print(X)
  tides_plot(tideStations[X, ], p.dir = plotDir)
})



# tides_plot <- function(metaDat, p.width = 4, p.height = 2.5, p.dir, datum=NULL){
metaDat = tideStations[1, ]
p.width = 4
p.height = 2.5
p.dir = paste0(outDir, "Tide_figs/")
datum=NULL
  
  # Determine midnight and noon for dates of this previous week
  day <- -5:7
  day_midnight <- as.POSIXct(paste0(Sys.Date() - day, "00:00"), 
                             format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  day_noon <- as.POSIXct(paste0(Sys.Date() - day, "12:00"), 
                         format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  
  # Download the recent and forecast water levels
  watLev = collectTideData(metaDat, vars="water_level", dates="recent")
  forecast = collectTideData(metaDat, vars="ofs_water_level", 
                             begin_date=format(Sys.Date(), "%Y%m%d"), 
                             range=72)
  
  ##create plot
  png(file=paste0(p.dir, "Fig_", metaDat$id, ".png"), family="Helvetica", units="in", 
      width=p.width, height=p.height, pointsize=12, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
  
  if(length(watLev$v) == 1 && is.na(watLev$v)){ # If no data
    plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    legend("center", "No data available", bg="white")
    
  } else { # if data
    # Format the date/time into an R object
    watLev$Date = as.POSIXct(watLev$t, format="%Y-%m-%d %H:%M", tz="")
    forecast$Date = as.POSIXct(forecast$t, format="%Y-%m-%d %H:%M", tz="")
    
    # Remove all forecast values prior to the present time frame
    lastObsTime <- watLev$Date[length(watLev$Date)]
    forecast <- forecast[-which(as.POSIXct(as.character(forecast$t)) < lastObsTime), ]
    
    # Find the range of both the time and values for plotting axis limits
    timernge = range(c(watLev$Date, forecast$Date), na.rm = TRUE)
    valrnge = range(c(as.numeric(as.character(watLev$v)), 
                      as.numeric(as.character(forecast$v))), na.rm = TRUE) 
    
    plot(0, type="n", ylab=paste0("Height (ft ", unique(watLev$datum), ")"), 
         xlab="Local standard time", xaxt="n", xlim = timernge, ylim = valrnge) #klr changed m to ft
    
    # Add some nice gridding
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    axis(1, at=day_midnight, labels=FALSE, tick=TRUE)
    axis(1, at=day_noon, labels=gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick=FALSE)
    grid(NA, NULL, lty=6, col="gray")
    abline(v=day_midnight, lty=6, col="gray")
    
    # Add the water level values and forecast line
    lines(watLev$Date, as.numeric(as.character(watLev$v)), lwd=2, col="steelblue")
    lines(forecast$Date, as.numeric(as.character(forecast$v)), lwd=2, lty=2, col="steelblue")
    abline(v=watLev$Date[length(watLev$Date)], lwd=2, col="black")
    
  }
  dev.off()
}




