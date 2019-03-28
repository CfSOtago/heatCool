# Script to run testReport using several options for data input

# set data input
# this will switch from Emily's Heat Pump data extraction to Ben's
dataSource <- "emilyData"
#dataSource <- "benData"

# What are we running on?
sysInfo <- Sys.info()
myParams <- list()
myParams$sysName <- sysInfo[[1]]
myParams$nodeName <- sysInfo[[4]]
myParams$userName <- sysInfo[[7]]

if(myParams$userName == "ben"){ # BA laptop
  # myParams$dPath <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/Self_contained_Projects/2018_coolingEmily/archive/") # HCS - requires VPN/on-campus connection
  myParams$dPath <- path.expand("~/Data/NZ_GREENGrid/safe/emilyJiang/") # copy of data on HCS
}else{
  myParams$dPath <- "Z:/Research Projects/GREEN Grid/Self_contained_Projects/2018_coolingEmily/archive/" # <- windoze
}

if(dataSource == "emilyData"){
  dataFile <- paste0(myParams$dPath, "allHouseholds_hourly.csv.gz") # Emily's full data extract
}
if(dataSource == "benData"){
  dataFile <- "~/Data/NZ_GREENGrid/safe/gridSpy/1min/dataExtracts/Heat Pump_2015-04-01_2016-03-31_observations.csv.gz" # data for 1 year extracted previously
}

# test file exists
if(file.exists(dataFile)){
  message("Success! Found expected pre-extracted file: ", dataFile)
} else {
  message("Opps. Didn't find expected pre-extracted file: ", dataFile)
  stop("Please check you have set the paths and filename correctly...")
}

rmdFile <- paste0(getwd(), "/analysis/testReport_v1.0.Rmd") # assumes you're running in the repo root - beware!

# load libs we need to run
library(rmarkdown)
library(bookdown)
rmarkdown::render(input = rmdFile,
                  output_format = "html_document2",
                  params = list(dataSource = dataSource, dataFile = dataFile),
                  output_file = paste0(getwd(), "/testReport_v1.0_", dataSource, ".html") # NB: this is relative to the .Rmd file
)