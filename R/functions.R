######################## Emily's FUNCTIONS ######################## 

# checks if a directory exists given its path and creates it
checkDirec <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  }
}

# input is the original weird gz file
getGz <- function(f){
  Tdf <- readr::read_csv(f,
                         col_types = cols(hhID = col_character(),
                                          linkID = col_character(),
                                          dateTime_orig = col_character(), # <- this is crucial otherwise readr attempts to parse this as a dateTime and FAILS
                                          TZ_orig = col_character(),
                                          r_dateTime = col_datetime(format = ""),
                                          circuit = col_character(),
                                          powerW = col_double() # <- also crucial otherwise readr seems to assume an integer
                         ),
                         progress = FALSE # no feedback
  )
  Tdt <- data.table(Tdf)
  return(Tdt)
}

# input is the original data frame. returns same data frame but only rows with heatpump in them. 
# outside of function, check if df has "heat pump" in circuit column
# seems that df can also be a dt
getHeatPump <- function(df){
  rows <- grep(pattern = "Heat Pump", x = df$circuit, ignore.case = T, value = F)
  heating <- df[rows,]     # creates df using only heatpump rows
  return(heating)
}

# returns the house ID given a string (str must be in the format "rf_#_etc" where # can be as many digits as you like i.e. ID must finish before second _)
getHouseId <- function(str){
  end_index <- unlist(gregexpr("_", str))[2]
  house_id <- substr(str, 1, end_index-1)
  return(house_id)
}

# removes negative and too large power values
removeOutliers<- function(dt){
  dt$powerW <- as.numeric(dt$powerW)
  dt$powerW[dt$powerW < 0] <- NA
  dt$powerW[dt$powerW > 10000] <- NA
  dt <- dt[which(!is.na(dt$powerW)),]
  return(dt)
}

## merge_heating functions take heating df and aggregates power usage to specified timeframe. 
# df cannot be a dt?
aggregateTime <- function(dt, str){
  df <- as.data.frame(dt)
  if(str == "hour"){
    t <- aggregate(df$powerW, list(df$circuit, hour(df$r_dateTime), date(df$r_dateTime)), mean)
    t$r_dateTime <- as.POSIXct(paste(t$Group.3, t$Group.2),format = "%Y-%m-%d %H", tz = "UTC") 
  } else if(str == "day"){
    t <- aggregate(df$powerW, list(df$circuit, date(df$r_dateTime)), mean)
    t$r_dateTime <- date(as.POSIXct(t$Group.2,format = "%Y-%m-%d"))
  } else if(str == "month"){
    t <- aggregate(df$powerW, list(df$circuit, month(df$r_dateTime), year(df$r_dateTime)), mean)
    t$r_dateTime <- as.POSIXct(paste(t$Group.3, t$Group.2, "1", sep = "-"),format = "%Y-%m-%d")   # date = first of month
  }
  colnames(t)[colnames(t)=="x"] <- "powerW"
  colnames(t)[colnames(t)=="Group.1"] <- "circuit"
  t <- t[c("r_dateTime", "powerW", "circuit")]
  return(as.data.table(t))
}

# function that goes changes the name of every entry of the circuit column in a given dt
cleanCircuitName <- function(dt){
  for(i in 1:length(dt$circuit)){
    dt$circuit[i] <- rmDollar(dt$circuit[i])
  }
  return(dt)
}

# function which removes characters after $
# used to clean up the circuit names
rmDollar <- function(str){
  i <- unlist(gregexpr(pattern = "\\$", "Heat Pump$4124"))
  varname <- substr("Heat Pump$4124", 1, i-1)
  return(varname)
}

gghist <- function(power, binval = 40, xmin = NULL, xmax = NULL,  axislab = F){ 
  p <- ggplot() + 
    geom_histogram(aes(power), colour = "black", fill = "white", breaks = seq(0, max(power), by = binval)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("") + ylab("") 
  if(is.infinite(max(power)))
    if(!missing(xmin) && !missing(xmax)){ #xmin and xmax specified
      p <- p + xlim(c(xmin, xmax))
    } else if(!missing(xmax)){ #xmax specified
      p <- p + xlim(c(0, xmax))
    } else if(!missing(xmin)){ #xmin specified
      p <- p + xlim(c(xmin, max(power)))
    }
  if(axislab == T){
    p <- p + xlab("Power W") + ylab("Count") 
  }
  return(p)
}

# sorry this is a very ugly function but it just extracts the dateTime, 
# max, min and mean temperatures from the CliFlo data from NIWA
getDateMaxMinMean <- function(dt){
  vari <- c("Station", "Date.NZST.", "Tmax.C.", "Tmin.C.", "Tmean.C.")
  newVari <- c("location", "r_dateTime", "Tmax_C", "Tmin_C", "Tmean_C")
  dt$Date.NZST. <- dmy_hm(dt$Date.NZST., tz = "NZ") 
  newdt <- dt[,..vari]
  for(i in 3:length(newdt)){
    newdt[[i]] <- as.numeric(newdt[[i]])
  }
  for(j in 1:length(newVari)){
    colnames(newdt)[j] <- newVari[j]
  }
  return(newdt)
}

#returns data of specified season
bySeason <- function(s, df){
  rows <- df$season == s
  dfSeason <- df[rows,]
  return(dfSeason)
}

#returns data of specifed hour
byHour <- function(h, df){
  rows <- hour(df$r_dateTime) == h
  dfHour <- df[rows,]
  return(dfHour)
}

#returns a df of mean temperatures values (based on vari variable e.g. = Tmean_C) vs year
getYearMean <- function(df, vari){
  yearMean <- aggregate(df[,..vari], list(year(df$r_dateTime)), mean, na.rm = T)
  names(yearMean) <- c("year", "mean_T_C")
  return(yearMean)
}

#returns a df of projected y values e.g. temperature vs years (need to specify initial year)
getTempProjection <- function(df, yearFinal = 2050){
  yearProjec <- c(df[1,1]:yearFinal)
  mod <- lm(df$mean_T_C~year,df)
  yint <- coef(mod)[1]
  m <- coef(mod)[2]
  projecVal <- m*yearProjec+yint
  projecDf <- data.frame(yearProjec, projecVal)
  names(projecDf) <- c("year", "projected_mean_T_C")
  return(projecDf)
}

# returns a string of the hour in format "00:00"
makeHourString <- function(n){
  time <- paste(n, "00", sep = ":")
  if(n <10){
    time <- paste("0", time, sep = "")
  }
  return(time)
}

# makes a boxplot from the hourly data using Tmean_C
makeTempBoxPlot <- function(df){
  p <- ggplot(mapping = aes(x = df$r_dateTime, 
                            y = df$Tmean_C, 
                            group = year(df$r_dateTime))) + 
    geom_boxplot()
  return(p)
}

# add somewhere to change the label on the legend
makeMeanGeom <- function(df, clr, legendLabel){
  gp <- geom_point(mapping = aes(x = df[,1], y = df[,2], colour = clr)) 
  return(gp)
}

# combines time and season for a plot title
makePlotTitle <- function(h, str){
  hstr <- makeHourString(h)
  plotTitle <- paste(str, hstr)
  return(plotTitle)
}

# because I cbf writing the code for only the "on" values every time
getOn <- function(dt){
  rows <- dt$powerW >= 40
  onDt <- dt[rows,]
  return(onDt)
}


# this just gives you the rows that you want according to hour and/or season
selectRowsByTime <- function(dt, theHour = NULL, theSeason = NULL){
  
  if(!missing(theHour) && missing(theSeason)){                # only hour specified
    theseRows <- (hour(dt$r_dateTime) == theHour)
    thisDt <- dt[theseRows,]
  }else if(missing(theHour) && !missing(theSeason)){          # only season specified 
    theseRows <- (dt$season == theSeason)
    thisDt <- dt[theseRows,]
  } else if(!missing(theSeason) && !missing(theSeason)){      # ideally both specified...
    hourRows <- (hour(dt$r_dateTime) == theHour)
    thatDt <- dt[hourRows,]
    seasonRows <- (thatDt$season == theSeason)
    thisDt <- thatDt[seasonRows,]
  }
  return(thisDt)
}

# makes a beautiful dt of |r_dateTime (NZST) | meanPowerON | meanPowerTotal | countON | countOFF | countNA |
goRogue <- function(dtList, theTime){
  
  countNA <- 0
  countOFF <- 0
  countON <- 0
  sumPower <- 0
  meanPowerON <- 0
  meanPowerTotal <- 0
  
  for(i in 1:length(dtList)){ # for each house, check for the row with the dateTime and apply these if statements
    dt <- dtList[[i]]                                     # getting dt
    thisRow <- dt$r_dateTime == theTime                   # getting row that corresponds to the correct dateTime
    if(nrow(dt[thisRow,]) == 0){
      countNA <- countNA + 1                              # adding to NA as that row doesn't exist
    }else if(dt[thisRow,]$powerW < 40){
      countOFF <- countOFF + 1                            # adding to OFF as HP is on standby/OFF at this time
    }else{
      sumPower <- sum(c(sumPower,dt[thisRow,]$powerW))    # summing power to be able to mean later
      countON <- countON + 1
    }
  }
  
  if(countON != 0){
    meanPowerON <- sumPower/countON                       # mean power of only ON houses
    meanPowerTotal <- sumPower/(sum(c(countON,countOFF))) # mean power of houses with data
  } else if(countNA != length(dtList)){
    meanPowerON <- NA                                     # if no ON data (OFF AND NA data)
    meanPowerTotal <- 0
  } else {
    meanPowerON <- NA                                     # if no data, mean power = NA
    meanPowerTotal <- NA
  }
  
  if(sum(countNA, countOFF, countON) == !length(dtList)){
    print(paste(theTime, " something wrong here"))        # just doing a bit of error checking
    
  }else{                    # if power data exists, return a row/data table
    return(data.table("r_dateTime" = theTime, 
                      "meanPowerON_W" = meanPowerON,
                      "meanPowerTotal_W" = meanPowerTotal,
                      "countON" = countON,
                      "countOFF" = countOFF,
                      "countNA" = countNA))
  }
  
}

getMeanPower <- function(dt, theTime){
  
  count.active <- 0
  count.on <- 0
  sum.power <- 0
  mean.on <- 0
  mean.active <- 0
  
  #search for each time
  timeRows <- dt$r_dateTime == theTime    # only time specific rows
  
  if(nrow(dt[timeRows,]) == 0) {
    return()
  } else {
    dt.time <- dt[timeRows,]
    
    count.active <- nrow(dt.time)    # no. of active houses
    
    # no. of ON houses
    onRows <- dt.time$powerW >= 40
    dt.on <- dt.time[onRows,]
    count.on <- nrow(dt.on)
    
    #mean of ON houses
    sum.power <- sum(dt.on$powerW)
    mean.on <- sum.power/count.on
    
    # mean of ACTIVE houses
    mean.active <- sum.power/count.active
    
    return(data.table("r_dateTime" = theTime, 
                      "mean_power_on_W" = mean.on,
                      "mean_power_active_W" = mean.active,
                      "count_on" = count.on,
                      "count_active" = count.active))
  }
}

# this is the function that actually makes the histogram plots
testForNormality <- function(dt, powerON = T, powerTotal = F){
  if(powerON){
    # use meanPowerON_W
    power <- dt$meanPowerON_W
    subt <- "ON ONLY"
  } else if(powerTotal){
    # use meanPowerTotal_W
    power <- dt$meanPowerTotal_W
    subt <- "ALL HOUSEHOLDS"
  } else {
    print("ya did something wack here")
  }
  # need to only incl. non NA values
  power <- power[!is.na(power)]
  
  title <- paste("Hour ", hour(dt$r_dateTime))
  p <- gghist(power = log(power), binval = 0.1) + 
    ggtitle(title, paste(dt$season, subt, sep = " - ")) +
    xlab("log(Power W)") + ylab("Count")
  return(p)
}

# returns a dt row of | temp | meanpower | medianpower |
getTempMeanMed <- function(dt, lower, upper){
  thisTempDt <- dt[lower <= dt$Tmean_C & dt$Tmean_C < upper,]
  theMean <- mean(thisTempDt$powerW, na.rm = T) # will give the mean of meanPower with NAs removed
  theMedian <- median(thisTempDt$powerW, na.rm = T) # will give the median " "  
  return(data.table("temp" = lower,
                    "meanPower_W" = theMean,
                    "medianPower_W" = theMedian))
}

# yvar and group need to be in the format 'dt$var'
# need to add the ylab outside of the funtion
scatter.temp <- function(dt, yvar, group = NULL){
  mainPlot <- ggplot(data = dt , mapping = aes(y = yvar, x = Tmean_C))
  
  if(missing(group)){     # colours the points if group is given
    gpoint <- geom_point(alpha = 0.7)
  }else{
    gpoint <- geom_point(alpha = 0.7, mapping = aes(colour = group))
  }
  
  finalPlot <- mainPlot + 
    gpoint +
    scale_x_continuous(breaks = seq(-10,40, 5)) +
    scale_y_continuous(breaks = seq(0, 8000, 500)) +
    the_theme +
    xlab("Mean Temperature (?C)") 
  
  return(finalPlot)
}
# yvar needs to be in the format 'dt$var'
# need to add the ylab outside of the funtion
boxplot.temp <- function(dt, yvar){
  mainPlot <- ggplot(data = dt , mapping = aes(y = yvar, x = Tmean_C))
  finalPlot <- mainPlot + 
    geom_boxplot(mapping = aes(group = cut_width(x = dt$Tmean_C, width = 1, boundary = 0))) +
    scale_x_continuous(breaks = seq(-10,40, 5)) +
    scale_y_continuous(breaks = seq(0,5000, 500)) +
    blank_theme  + 
    xlab("Mean Temperature (?C)")
  
  return(finalPlot)
}

# shifts the power values down 40 W to reflect only the power used for heating/cooling
# ok this probably could have been weaved in when removing the 40W values but oh well
standbyAdjust <- function(dt){
  newdt <- dt
  newdt$powerW <- dt$powerW - 40
  return(newdt)
}

# retrieve or exclude only the rows of a certain house
getIDRows <- function(dt, id, rm = F){
  rows <- grep(pattern = id ,x = dt$id, ignore.case = T)
  if(rm){
    # remove the rows 
    newdt <- dt[-rows,]
  }else{
    # extract the rows
    newdt <- dt[rows,]
  }
  return(newdt)
}

# retrieve or exclude only the rows of a certain circuit
getCircuitRows <- function(dt, circuit, rm = F){
  rows <- grep(pattern = circuit ,x = dt$circuit, ignore.case = T)
  if(rm){
    # remove the rows 
    newdt <- dt[-rows,]
  }else{
    # extract the rows
    newdt <- dt[rows,]
  }
  return(newdt)
}

# produces a data table with modelled mean and median power consumption 
# based on mean and median power consumption vs mean temperature
getQuadModel <- function(dt, minTemp, maxTemp){
  step = 0.5
  
  if(missing(minTemp)| missing(maxTemp)){ 
    indexStep <- 5/step
    temperatures <- seq(0, 30, step)
    modelDt <- rbindlist(lapply(temperatures, function(l){
      return(getTempMeanMed(dt, l, l+step))
    }))
    i <- which.min(modelDt$medianPower_W)
    thisModelDt <- modelDt[seq(i-indexStep, i+indexStep),]
  }else{
    thisModelDt <- rbindlist(lapply(seq(minTemp, maxTemp, step), function(l){
      return(getTempMeanMed(mergedfinal, l, l+step))
    }))
  }
  
  tempMod <- poly(thisModelDt$temp, 2)
  
  thisModelDt$medianPower_mod <- fitted(lm(thisModelDt$medianPower_W~tempMod))
  thisModelDt$meanPower_mod <- fitted(lm(thisModelDt$meanPower_W~tempMod))
  
  return(thisModelDt)
}
