# process the data
# based on code in analysis/final.R

source("R/libraries.R") # Emily's recommended libraries
source("R/functions.R") # Emily's functions

# get data from Otago HCS
direc <- "Z:/Research Projects/GREEN Grid/Self_contained_Projects/2018_coolingEmily"

location <- c(15876, 23872)

ssaw <- factor(c("Spring", "Summer", "Autumn", "Winter")) # list of seasons
timeRange <- seq(as.POSIXct("2014-01-01 00:00:00"), as.POSIXct("2019-01-01 00:00:00"), by = "hour") # lists dateTimes that will be in the meanCountList

allHouseholds <- data.table::data.table()
allTemp <- data.table::data.table()



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