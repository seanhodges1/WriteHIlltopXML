## ----------------------------------------------------------------------------
## Load laboratory csvs to dataframe and make available to process into XML
## ----------------------------------------------------------------------------

## Load libraries ------------------------------------------------
## uses base functions

## Load Functions ------------------------------------------------

# For the Uncertainty column in the csvs - Need a way to ensure that always has a length of 2
# once split. Data is expected in the form of 'xx.xx-yy.yy or variants of that
# Replaces single empty string resulting from strsplit() with character vector of length 2
removeEmpties <- function(x){
  if(length(x)<=1) {
    x<-c("0","0") 
  } else {
    x
  }
}


#### --- Now the body of the code ---
setwd("Z:/data/RScript/LaboratoryCSVs")
setwd("//file/herman/O/IT/06/02/SourceCode/RScripts/LaboratoryCSVs")
#df <- read.csv("//file/herman/R/OA/08/02/2015/Water Quality/Horizons/DataPreparation/CSVfilesToProcess.csv",stringsAsFactors=FALSE,)
#df <- read.csv("//file/herman/R/OA/08/02/2015/Water Quality/Horizons/DataPreparation/CSVQuarterlyfilesToProcess.csv",stringsAsFactors=FALSE,)
df <- read.csv("//file/herman/O/IT/06/02/SourceCode/RScripts/LaboratoryCSVs/watercare.csv",stringsAsFactors=FALSE)

# get a list of all the log files
fileNames <- as.vector(df[,2])



# This line
# 1) reads each csv file
# 2) concatenates them

# Before LabCSVs dataframe is written, need some way to add the source file name against each row. I wonder
# if this can be done through the  read?

data <- do.call( "rbind", lapply( fileNames, function(fn)
            data.frame(Filename=fn, filetime=file.mtime(fn), read.csv(fn, sep = ",", header=FALSE, skip=1,stringsAsFactors=FALSE))[1:19]
        ))


#names(data) <- c("filename","filetime","batchno","sampleID","receivedDate","dateReported","tcode","ltgt","value","result","units","status","method",
#                 "tempOnArrival","detectionLimit","uncertainty","sampleDate","sampleDescription","testDescription","Cost")

names(data) <- c("filename","filetime","batchno","sampleID","receivedDate","dateReported","tcode","ltgt","value","result","units","status","method",
                 "tempOnArrival","detectionLimit","uncertainty","sampleDate","sampleDescription","testDescription")



## The following cleanup required for ESL data, but not WaterCare

## Need to clean up the uncertainty column before splitting. 
## Turns out that it contains
##    1. Spaces
##    2. Question marks ??????
##    3. No values in places
##    4. N/A and NA

# 1. Letters
#data$uncertainty<-gsub(pattern = "[a-zA-Z]*",replacement = "",x = data$uncertainty)
# 2. Spaces
#data$uncertainty<-gsub(pattern = "\\s",replacement = "",x = data$uncertainty)
# 3. Slashes
#data$uncertainty<-gsub(pattern = "\\/",replacement = "",x = data$uncertainty)
# 4. Question marks
#data$uncertainty<-gsub(pattern = "\\?",replacement = "",x = data$uncertainty)


## split uncertainties into low and high values
## returns a list than is then converted to a data.frame
## and then bound to original dataframe using cbind.

#uncertainty <- lapply(strsplit(data$uncertainty,split="(?<=[0-9])\\-",perl=TRUE),removeEmpties)   ## RegExp matches hyphen preceded by a number

uncertainty <- lapply(strsplit(data$uncertainty,split=" - ",perl=TRUE),removeEmpties)   ## RegExp matches hyphen with space before and after

u <-as.data.frame(matrix(unlist(uncertainty),ncol=2,byrow=TRUE),stringsAsFactors=FALSE)
names(u) <- c("lowerCI","upperCI")
data <- cbind(data,u)
rm("u","uncertainty")
data$filepath<-data$filename
data$SampleID<-as.numeric(data$sampleID)

data<-data[-1] ## dropping the filename at the start of the dataframe
# data rearrangement for ESL
#data<-data[c(2,23,3:15,20:21,16:19,22,1)]  # rearranging columns to put lower and upper CI next to uncertainty.

# data rearrangement for WaterCare
data<-data[c(2,22,16,4:15,19:20,17:18,21,1)]  # rearranging columns to put lower and upper CI next to uncertainty.

# Remove fields that have ambiguous date formats that will upset finding unique values or unique rows
# -- namely receivedDate [4],dateReported [5] and sampleDate [18]
#data<-data[-18]
data<-data[-5]
data<-data[-4]

# Optional rewrite of single CSV of all data - useful for reviewing data
# Takes about 5 minutes to complete writing file.
write.csv(data,"//file/herman/O/IT/06/02/SourceCode/RScripts/LaboratoryCSVs/watercareData.csv")

