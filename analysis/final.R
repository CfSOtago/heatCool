# Created 19-12-2019. 
# Emily Jiang

# NB the seasonHourTempList has 4 entries: one for each season. Each entry is a list corresponding to each hour
# in the day, however the indexing is one greater than the hour e.g. seasonHourTempList[[1]][[1]] = spring 00:00
# NB the usage of double brackets
library(devtools)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(data.table)
library(GREENGridData)
library(viridis)
library(ggforce)

######################## FUNCTIONS ######################## 

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


direc <- "Z:/Research Projects/GREEN Grid/Self_contained_Projects/2018_coolingEmily"

location <- c(15876, 23872)

ssaw <- factor(c("Spring", "Summer", "Autumn", "Winter")) # list of seasons
timeRange <- seq(as.POSIXct("2014-01-01 00:00:00"), as.POSIXct("2019-01-01 00:00:00"), by = "hour") # lists dateTimes that will be in the meanCountList

allHouseholds <- data.table()
allTemp <- data.table()


#################### The pre bit, not sure if you need any of it for further processing #####

####################
st <- Sys.time()
####################
for(loc in 1:length(location)){
  
  ############################################# circuit main ############################################
  direcGz <- file.path(direc, "data_copy/to_merge_heating", location[loc])
  direcHP <- file.path(direc, "data_copy/heat_pump_by_time")
  
  direcCSV <- file.path(direcHP, "Spreadsheets")
  direcPlots <- file.path(direcHP, "Plots2")
  
  # gets names of original household data
  gzFileList <- unlist(lapply(direcGz, list.files, pattern = ".gz"))   # sapply(direcGz, list.files, pattern = ".gz")[1] returns the gz file name, # sapply(direcGz, list.files, pattern = ".gz")[1,] returns the path of the gz file
  idList <- unlist(lapply(gzFileList, getHouseId))
  
  setwd(direcGz)
  
  # makes a list with each entry containing a df of the heatpump data (min).
  heatPumpDfMin <- lapply(gzFileList, function(gz){
    Tdf <- getGz(gz)
    heat <- getHeatPump(Tdf)
    heat <- removeOutliers(heat) #removes below 0 and above 10000 W power values for the minute
  }
  ) #worked bc I set it as a datatable I think
  
  # creates df of houly hp data, each entry representing a household
  heatPumpDfHour <- lapply(heatPumpDfMin, 
                           aggregateTime, 
                           str = "hour")
  
  # adds column with id
  for(i in 1:length(heatPumpDfHour)){
    heatPumpDfHour[[i]]$id <- idList[i]
  }
  
  # make into a big dt with all houses in one dt
  heatPumpDfHourAll <- rbindlist(heatPumpDfHour)
  heatPumpDfHourAll <- cbind(heatPumpDfHourAll, "location" = location[loc])
  allHouseholds <- rbind(allHouseholds, heatPumpDfHourAll)
  
  ########################################## weather stuff ##########################################
  weatherDirec <- file.path(direc, "NIWA", location[loc])
  setwd(weatherDirec)
  
  
  temp <- as.data.table(read.table(paste(location[loc], "_hourly_all.txt", sep = ""), header = T, sep = ",", stringsAsFactors = F) )
  temp <- getDateMaxMinMean(temp)
  temp <- temp[!duplicated(temp),] # removing repeated rows
  temp <- cbind(temp, "location" = location[loc])
  allTemp <- rbind(allTemp, temp)
}

temp <- as.data.table(read.table("test.txt"), header = T, sep = ",", stringsAsFactors = F)


setwd(file.path(direc, "data_copy"))
write.csv(x = allHouseholds, file =  "allHouseholds_hourly.csv")
write.csv(x = allTemp, file =  "alltemp_hourly.csv")

####################
pt <- Sys.time()- st
####################

########################## Where the further processing happens ###############################

# reading the files that should've been saved :)
setwd(file.path(direc, "data_copy"))
allHouseholds <- read.csv(file = "allHouseholds_hourly.csv", header = T, stringsAsFactors = F)
allTemp <- read.csv(file = "alltemp_hourly.csv", header = T, stringsAsFactors = F)

allHouseholdsCircuitList <- unique(allHouseholds$circuit) #provides a list of circuit names of allHouseholds

merged <- as.data.table(inner_join(allHouseholds, allTemp, by = c("location", "r_dateTime"))) #merged P and T data by dateTime and location
mergedON <- getOn(merged)  # only entries where power is greater than or equal to 40 W
mergedON.shifted <- standbyAdjust(mergedON) # adjusting for standby power i.e. removing 40 W
addNZSeason(mergedON.shifted, r_dateTime)

#### removing unwanted circuits ####
circuitRows2212 <- which(mergedON.shifted$circuit == "Downstairs (inc 1 Heat Pump)$2212")
circuitRows2741 <- which(mergedON.shifted$circuit =="Bedroom & Lounge Heat Pumps$2741")
circuitRows2740 <- which(mergedON.shifted$circuit =="Theatre Heat Pump$2740")
####################################

mergedfinal <- mergedON.shifted[-c(circuitRows2212, circuitRows2741, circuitRows2740), ] # the final clean version?

# I think this plots a facet grid (for each location) of boxplots 
# with temperature on the x and power on the y
ggplot(data = mergedfinal[!is.na(Tmean_C)], mapping = aes(x = Tmean_C, y = powerW)) + 
  geom_boxplot(mapping = aes(cut_width(Tmean_C, 1, boundary = 0)))  +
  facet_grid(location~.)
#coord_cartesian(xlim = c(10,30), ylim = c(0, 1500))

######## Making Tmean_C plots by location and total ######## 

for(j in 1:length(location)){
  loc <- location[j]
  dat <- mergedfinal[mergedfinal$location == loc,]
  p_name <- paste(paste(loc, "Tmean_C", sep = "_"), "png", sep = ".")
  p_name <- paste("test", p_name, sep = "_")
  
  p <- ggplot(data = dat, mapping = aes(x = Tmean_C, y = powerW))
  pp <- p + geom_boxplot(mapping = aes(group = cut_width(Tmean_C, 1, boundary = 0)))
  
  ggsave(filename = p_name, 
         plot = pp, 
         device = "png", 
         path = file.path(direc, "report_plots"), 
         width = 15, 
         height = 15, 
         units = "cm")
  
}

p_name <- paste("total_Tmean_C", "png", sep = ".")
p_name <- paste("test", p_name, sep = "_")

p <- ggplot(data = mergedfinal, mapping = aes(x = Tmean_C, y = powerW))
pp <- p + geom_boxplot(mapping = aes(group = cut_width(Tmean_C, 1, boundary = 0)))

ggsave(filename = p_name, 
       plot = pp, 
       device = "png", 
       path = file.path(direc, "report_plots"), 
       width = 15, 
       height = 15, 
       units = "cm")

######## ######## ######## ######## ######## 

######## Making Tmax_C plots by location and total ######## 

for(j in 1:length(location)){
  loc <- location[j]
  dat <- mergedfinal[mergedfinal$location == loc,]
  p_name <- paste(paste(loc, "Tmax_C", sep = "_"), "png", sep = ".")
  p_name <- paste("test", p_name, sep = "_")
  
  p <- ggplot(data = dat, mapping = aes(x = Tmax_C, y = powerW))
  pp <- p + geom_boxplot(mapping = aes(group = cut_width(Tmax_C, 1, boundary = 0)))
  
  ggsave(filename = p_name, 
         plot = pp, 
         device = "png", 
         path = file.path(direc, "report_plots"), 
         width = 15, 
         height = 15, 
         units = "cm")
  
}

p_name <- paste("total_Tmax_C", "png", sep = ".")
p_name <- paste("test", p_name, sep = "_")

p <- ggplot(data = mergedfinal, mapping = aes(x = Tmax_C, y = powerW))
pp <- p + geom_boxplot(mapping = aes(group = cut_width(Tmax_C, 1, boundary = 0)))

ggsave(filename = p_name, 
       plot = pp, 
       device = "png", 
       path = file.path(direc, "report_plots"), 
       width = 15, 
       height = 15, 
       units = "cm")

######## ######## ######## ######## ######## 


####### Tmean_C facet wraps #######

r <- 3 #nrows
c <- 2 #ncols

for(j in 1:length(location)){
  loc <- location[j]
  
  dat <- mergedfinal[mergedfinal$location == loc,]
  
  n_pages <- ceiling(length(unique(dat$circuit))/(r*c))
  
  for(i in 1:(n_pages)){
    p_name <- paste(paste(loc, "Tmean_C", "page", i, sep = "_"), "png", sep = ".")
    p_name <- paste("test", p_name, sep = "_")
    
    p <- ggplot(data = dat, mapping = aes(x = Tmean_C, y = powerW))
    pp <- p +
      geom_boxplot(mapping = aes(group = cut_width(Tmean_C, 1, boundary = 0))) + 
      facet_wrap_paginate(facets = vars(circuit), nrow = r, ncol = c, page = i)
    
    ggsave(filename = p_name, 
           plot = pp, 
           device = "png", 
           path = file.path(direc, "report_plots"), 
           width = 15, 
           height = 20, 
           units = "cm")
  }
}

######## ######## ######## ######## ######## 


####### Tmax_C facet wraps #######

r <- 3 #nrows
c <- 2 #ncols

for(j in 1:length(location)){
  loc <- location[j]
  
  dat <- mergedfinal[mergedfinal$location == loc,]
  
  n_pages <- ceiling(length(unique(dat$circuit))/(r*c))
  
  for(i in 1:(n_pages)){
    p_name <- paste(paste(loc, "Tmax_C", "page", i, sep = "_"), "png", sep = ".")
    p_name <- paste("test", p_name, sep = "_")
    
    p <- ggplot(data = dat, mapping = aes(x = Tmax_C, y = powerW))
    pp <- p +
      geom_boxplot(mapping = aes(group = cut_width(Tmax_C, 1, boundary = 0))) + 
      facet_wrap_paginate(facets = vars(circuit), nrow = r, ncol = c, page = i)
    
    ggsave(filename = p_name, 
           plot = pp, 
           device = "png", 
           path = file.path(direc, "report_plots"), 
           width = 15, 
           height = 20, 
           units = "cm")
  }
}

######## ######## ######## ######## ######## 

############################################# combining stuff  #############################################
### we end up with three (main) data tables: merged, mergedMean, mergedON
## merged: every active value from all the houses of the location
## mergedMean: finds the mean power values across all the houses at a certain hour (mean all) as
## well as the mean across all the houses that have their heat pump ON at that certain hour (mean on)
## mergedON: same as the merged dt, however has been filtered to only ON heat pumps
### the mergedON will be adjusted to account for the standby power, therefore representing
### only the power used to heat/cool i.e. powerW - 40 = heat/cool power

### The mergedfinal is the one where the model calculations will be based on.
### It has the power values shifted so that the values represent only the power used
### for heating/cooling as well as removing any weird outliers i.e rf_19



#mergedMean <- rbindlist(lapply(timeRange, getMeanPower, dt = heatPumpDfHourAll))
#mergedMean <- as.data.table(inner_join(mergedMean, temp, by = "r_dateTime"))
#addNZSeason(mergedMean, r_dateTime)

if(location == 23872){
  
  mergedON.rf19 <- getIDRows(mergedON.shifted, "rf_19", rm = F)  # making a rf_19 only dt
  mergedON.rf13 <- getIDRows(mergedON.shifted, "rf_13", rm = F)  # making a rf_13 only dt
  
  mergedON.excl.rf19.shifted <- getIDRows(mergedON.shifted, "rf_19", rm = T)    # removes rf_19 bc they skew the data
  mergedfinal <- getCircuitRows(mergedON.excl.rf19.shifted, circuit = "inc 1 Heat Pump", rm = T) # removes the downstairs circuit of rf_13 bc not just heat pump
}




# ok so here, we would extract the mean for each hour, or not. then plot. or get some useful stats?
# I'm not sure what useful stats are....
# shift the axis?
# need to make the bins different in boxplots so they're not centred to the nearest whole number - HOW??????




############################################ quadratic model #########################

## function for making a model for the mean
## function for making a model for the median
# requires mergedfinal, step width? 
# specifies the range which the model encompasses
# does it write the statistical values, coefficients within the function?
# returns a datatable

if(location == 23872){
  modelDt <- getQuadModel(mergedfinal, 10, 20)
}

if(location == 15876){
  modelDt <- getQuadModel(mergedfinal, 10, 20)
}
# saves the model information into a text file - porbably should specificy a save location :)
setwd(direc)
sink(paste("models_and_stats", paste(location, "txt", sep = "."), sep = "_"))
cat("Models for range:",min(modelDt$temp)," to", max(modelDt$temp), "degrees Celsius")
cat("\n\nModelling Median\n")
summary(lm(modelDt$medianPower_W~poly(modelDt$temp,2)))
cat("\nModelling Mean\n")
summary(lm(modelDt$meanPower_W~poly(modelDt$temp,2)))
sink()

# plots the quadratic models - DON'T KNOW HOW TO ADD A LEGEND!>!!>>!!
modPlot <- mergedfinalPlot + geom_boxplot(aes(group = cut_width(mergedfinal$Tmean_C, 1, boundary = 0))) + 
  geom_smooth(modelDt, mapping = aes(temp, meanPower_mod),colour = colourFit2, show.legend = T)+
  geom_smooth(modelDt, mapping = aes(temp, medianPower_mod), colour = colourFit3, show.legend = T) +
  coord_cartesian(xlim = c(min(modelDt$temp), max(modelDt$temp)), ylim = c(0, 1500)) +
  scale_y_continuous(breaks = seq(0, 8000, 250)) +
  the_theme +
  xlab(xlab.meanT) + 
  ylab(ylab.hourly)

ggsave(filename = paste("models_", location, ".png", sep = ""),
       plot = modPlot,
       device = "png", 
       path = file.path(direc, "report_plots"), 
       width = plotLength, height = plotLength, units = "cm")

############## spline modelling #################
d <- 0.5 # bin width of temperature

i <- -5    # lowest mean temperature
f <- 12 # higherst mean temperature (minus the binwidth)
lowerBound<- seq(i, f, d)

ii <- 12    # lowest mean temperature
ff <- 18 # higherst mean temperature (minus the binwidth)
lbb <- seq(ii, ff, d)

iii <- 18    # lowest mean temperature
fff <- 35 # higherst mean temperature (minus the binwidth)
lbbb <- seq(iii, fff, d)

# see getTempMeanMed function - makes into a dt for all temps
test <- rbindlist(lapply(lowerBound, function(l){
  return(getTempMeanMed(mergedfinal, l, l+d))
}))

test2 <- rbindlist(lapply(lbb, function(l){
  return(getTempMeanMed(mergedfinal, l, l+d))
}))

test3 <- rbindlist(lapply(lbbb, function(l){
  return(getTempMeanMed(mergedfinal, l, l+d))
}))


# remove rows with na/nan
test <- test[!is.na(test$meanPower_W) & !is.na(test$medianPower_W),]
test2 <- test2[!is.na(test2$meanPower_W) & !is.na(test2$medianPower_W),]
test3 <- test3[!is.na(test3$meanPower_W) & !is.na(test3$medianPower_W),]

# trying to make models but not sure if this is the right direction
tt <- poly(test$temp, 1)
test$meanPowerMod <- fitted(lm(test$meanPower_W~tt))
test$medianPowerMod <- fitted(lm(test$medianPower_W~tt))

tt <- poly(test2$temp, 2)
test2$meanPowerMod <- fitted(lm(test2$meanPower_W~tt))
test2$medianPowerMod <- fitted(lm(test2$medianPower_W~tt))

tt <- poly(test3$temp, 1)
test3$meanPowerMod <- fitted(lm(test3$meanPower_W~tt))
test3$medianPowerMod <- fitted(lm(test3$medianPower_W~tt))

theTest <- rbind(test, test2, test3)

x <- theTest$temp
y <- theTest$medianPowerMod
sx <- spline(x, y, n = 4)$x
sy <- spline(x, y, n = 4)$y
splinedt <- as.data.table(cbind.data.frame(sx, sy))
p + geom_smooth(data = splinedt, aes(sx, sy), colour = "red")

p + geom_line(theTest, mapping = aes(temp, meanPowerMod), colour = "red") +
  geom_line(theTest, mapping = aes(temp, medianPowerMod), colour = "blue")

p + geom_smooth(theTest, mapping = aes(temp, meanPowerMod), se = F, colour = "red") +
  geom_smooth(theTest, mapping = aes(temp, medianPowerMod), se = F, colour = "blue")

############# EVERYTHING FROM HERE ON IS PROBABLY UNNECESSARY #############

######################### ok so this bit just makes a bunch of histograms showing the log distributions #########
### oh crap they might not work if I get rid of meanCountList but may be able to work with the new dt?????
sp <- "Z:/Research Projects/GREEN Grid/Self_contained_Projects/2018_coolingEmily/data_copy/heat_pump_by_time/01-17"

for(s in levels(ssaw)){
  hourList <- list()
  for(h in 0:23){
    theDt <- selectRowsByTime(meanCountList, h, s)
    hourList[[h+1]] <- testForNormality(theDt)
    ggsave(filename = paste("ON", s, h, ".png", sep = ""), 
           plot = testForNormality(theDt), 
           device = "png", 
           path = sp,
           height = 6,
           width = 7,
           units = "cm")
    ggsave(filename = paste("TOTAL", s, h, ".png", sep = ""), 
           plot = testForNormality(theDt, powerON = F, powerTotal = T), 
           device = "png",
           path = sp,
           height = 6,
           width = 7,
           units = "cm")
  }
}
####################################### makes the bw plots #####################
test <- list()
for(s in 1:length(ssaw)){
  hourList <- list()
  for(h in 0:23){
    theDt <- selectRowsByTime(merged, h, ssaw[s])
    ggsave(filename = paste("ON", ssaw[s], h, "bin1", "bw.png", sep = "_"), 
           plot = makeBW(theDt, binwidth = 1, toLog = F), # not logged ON, 
           device = "png", 
           path = file.path(sp, "15876_bw"),
           height = 6,
           width = 7,
           units = "cm")
    ggsave(filename = paste("TOTAL", ssaw[s], h, "bin1", "bw.png", sep = "_"), 
           plot = makeBW(theDt, powerON = F, powerTotal =  T, binwidth = 1, toLog = F), # not logged TOTAL, 
           device = "png",
           path = file.path(sp, "15876_bw"),
           height = 6,
           width = 7,
           units = "cm")
    ggsave(filename = paste("ON", ssaw[s], h, "bin2", "bw.png", sep = "_"), 
           plot = makeBW(theDt, binwidth = 2, toLog = F), # not logged ON, 
           device = "png", 
           path = file.path(sp, "15876_bw"),
           height = 6,
           width = 7,
           units = "cm")
    ggsave(filename = paste("TOTAL", ssaw[s], h, "bin2", "bw.png", sep = "_"), 
           plot = makeBW(theDt, powerON = F, powerTotal =  T, binwidth = 2, toLog = F), # not logged TOTAL, 
           device = "png",
           path = file.path(sp, "15876_bw"),
           height = 6,
           width = 7,
           units = "cm")
    #hourList[[h+1]] <- p
  }
  #test[[s]] <- hourList
}

# yeah this looks like a shit function but we'll just leave it here for now until next week?
makeBW <- function(dt, powerON = T, powerTotal = F, toLog = F, binwidth = 1){ 
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
  if(toLog){
    power <- log(power)
  }
  title <- paste("Hour ", hour(dt$r_dateTime))
  p <- plot_bw(dt$Tmean_C, power, binwidth = binwidth) + ggtitle(title, paste(ssaw[s], subt, sep = " - ")) +
    xlab("Tmean C") + ylab("Power W")
  return(p)
}

# probably don't need this unless you are doing individual hours???? idk???
plot_bw <- function(xx,yy, binwidth = 1){
  bw <- ggplot(mapping = aes(xx, yy)) + geom_boxplot(aes(group = cut_width(xx, binwidth))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  return(bw)
}

### I don't think I need this ###
############################################
meanCountList <- rbindlist(lapply(timeRange,FUN = goRogue, dtList = heatPumpDfHour))  # makes that big dt
addNZSeason(meanCountList, r_dateTime)

# each entry represents a household
mergedList <- lapply(heatPumpDfHour, function(dt, tempDat){
  inner_join(dt, tempDat, by = "r_dateTime")
}, temp)


for(i in 1:length(mergedList)){
  mergedList[[i]]$id <- idList[i]
}
##################### DEAL WITH RF_19 (INDEX = 4) 

finalMergedList <- rbindlist(mergedList) # don't call it a list dumbass

# this is one giant df with means across all the households - see goRogue
merged <- inner_join(meanCountList, temp, by = "r_dateTime")

########### Showing individual houses boxplots to show that some houses show cooling and some don't. 
##Maybe show the proportion of houses that do and don't??? ###########
