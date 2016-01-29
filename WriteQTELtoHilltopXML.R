## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
mowSecs <- function(x){
  s<-strptime("1940-01-01","%Y-%m-%d", tz="GMT")  # USING GMT to avoid daylight time offset as default
  if(nchar(x)>=13){                               # arguments assume time based on NZST and NZDT for
    t<-strptime(x,"%d/%m/%Y %H:%M", tz="GMT")     # different parts of the year.
  } else {                                        # Will need to be aware of this for other work.
    t<-strptime(x,"%d/%m/%Y", tz="GMT")
  }
  x<-(t-s)*86400
  return(x)
}


## Load libraries ------------------------------------------------
require(RODBC)   ### ODBC library for SQL connection
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(XML)     ### XML library to write hilltop XML



# SQL connection - OPEN
conn <- odbcDriverConnect('driver={SQL Server};server=dbserver;database=mwrcdb;trusted_connection=true')

# SQL query to dataframes

# Query pulls sample_id, sample_procudure, collectedBy, comments, sample_number from QUALARC Tables
SampleComm    <- sqlQuery(conn,
                          paste("SELECT [sample_ID]",
                                ",isnull([sample_procedure],'') as [sampleProcedure]", 
                                ",CASE ",
                                "WHEN isnull([surname],'')='' OR ISNULL([first_names],'')=''",
                                "THEN ''",
                                "ELSE ISNULL([surname],'') + ', ' + ISNULL([first_names],'') ",
                                "END as [collectedBy]",
                                ",replace(replace(replace(cast([comments] AS nvarchar), char(10), ' | '), char(13), ' | '), char(9), ' | ') AS [comment]",
                                ",[sample_number]",
                                "FROM dbo.lab_sample",
                                "LEFT OUTER JOIN dbo.cont_staff_vapp ON",
                                "dbo.lab_sample.collected_by_id = dbo.cont_staff_vapp.agent_id",
                                "LEFT OUTER JOIN dbo.lab_sampling_procedure ON",
                                "dbo.lab_sample.sample_procedure_id = dbo.lab_sampling_procedure.sample_procedure_id",
                                sep=" "),
                          stringsAsFactors=FALSE
)


# SQL connection - CLOSE
close(conn)
rm(conn)

# renaming sample_id to match sampleid later on for a left-join
SampleComm$SampleID <- SampleComm$sample_ID

# reorder columns and remove redundant ones
# SampleID,sampleProcedure,collectedBy,Comments
SampleComm <- SampleComm[c(6,2:4)]

# --------------------------------------------------------
# Get QETL CSV
# The following code depends on the following file containing the data structured as showed in the following column headers
# HSiteName  SampleID  Date	JobCode	CostCentre	AuthNum	QMethodID	QMethodDescription	Qdetection	Qlab	HMeasurement	HDatasource	HFormat	HUnits	Raw_Value	Project	QSiteID
# 
# Field names prefixed by "H" are from Hilltop, and those by "Q" from Qualarc.
# Not all fields are prefixed. Those that aren't are from Qualarc as well.
# This file is transformed output of the WQ Data Export routine in
# \\ares\Environmental Data Validation\Discrete Water Quality\QETL\wqTransfer.xlsm

dospath <- "//ares/Environmental Data Validation/Discrete Water Quality/QETL/QETL200/QETL200.2toHilltopXML.csv"
dospath <- "M:/QETL200.3toHilltopXML.csv"
#dospath <- "//ares/Environmental Data Validation/Discrete Water Quality/QETL/QETL201/QETL201ToHilltopXML.txt"

#dospath <- "z:/data/RScript/LaboratoryCSVs/QETL202ToXML.csv"

datatbl<-read.delim(dospath, sep=",",stringsAsFactors=FALSE) 

datatbl$mowsecs<-mowSecs(datatbl$Date)
##               ^^^^^^^ Just calling the function across the column in datatbl results in NAs for dates like 18/05/1990

## Apply the mowSec function item by item works fine (just takes a lot longer) 
for(i in 1:length(datatbl[,1])){
  datatbl$mowsecs[i] <- mowSecs(datatbl$Date[i])
}

# Sort dataset by SiteName, Measurement and Date
# Make sure there is a trailing "," comma after the order() function, otherwise this will throw an error.
datatbl<-datatbl[order(datatbl$HSiteName,datatbl$HMeasurement,datatbl$mowsecs,datatbl$Project),]


## Apply Quality Code assumption before revision in DAta Cleanse step
datatbl$QC<-600

## DATA CLEANSING START ##------------------------------------


## Need to clean up the raw_value column before splitting. 
## Turns out that it contains
##    1. Pluses
##    2. N/A and NA AND NR and N/R  i.e. starts with N  --> set values to * and quality code 100
##    3. Values started with # -> These values need to quality coded 400
##    4. Values started with ? -> These values need to quality coded 400

## ## Keep original raw_value and date_time ## ##

datatbl$QRaw_Value <- datatbl$Raw_Value
datatbl$QDate      <- datatbl$Date
datatbl$Qmowsecs   <- datatbl$mowsecs

# 1. Plusses
#Finding strings ending with "+" and set quality code to 600
l<-grepl(pattern = "\\+$",x =  datatbl$Raw_Value,perl = TRUE)
cat("Fix BDISC trailing plus signs\n")
cat("Items: ",length(l))
cat("Found: ",sum(l))
datatbl$Raw_Value[l==TRUE]<-paste(">",gsub(pattern = "\\+$", replacement = "", x = datatbl$Raw_Value[l==TRUE]),sep="")
datatbl$QC[l==TRUE]<-600
# a<-subset(datatbl,grepl(pattern = "\\+$",x =  datatbl$QRaw_Value,perl = TRUE))

# 2. N/A's etc
#Finding strings starting with "N"
m<-grepl(pattern = "^N",x =  datatbl$Raw_Value,perl = TRUE)
cat("Fix Strings like N/R, NR, etc\n")
cat("Items: ",length(m))
cat("Found: ",sum(m))

# change values to "*" and set quality code to 100
datatbl$Raw_Value[m==TRUE]<-"*"
datatbl$QC[m==TRUE]<-100

# Quality Coding all asterixes (pre-existing plus the just converted) to 100
ast<-grepl(pattern = "^\\*",x =  datatbl$Raw_Value,perl = TRUE)
datatbl$QC[ast==TRUE]<-100


# Check changes
# a<-subset(datatbl,grepl(pattern = "^N",x =  datatbl$QRaw_Value,perl = TRUE))

# 3. Hashes
#Finding strings starting with "#"
n<-grepl(pattern = "^#",x =  datatbl$Raw_Value,perl = TRUE)
cat("Fix leading hash symbols\n")
cat("Items: ",length(n))
cat("Found: ",sum(n))

# Remove  hash symbol and set quality code to 400
datatbl$Raw_Value[n==TRUE]<-gsub(pattern = "^#", replacement = "", x = datatbl$Raw_Value[n==TRUE])
datatbl$QC[n==TRUE]<-400

# Check changes
# a<-subset(datatbl,grepl(pattern = "^#",x =  datatbl$QRaw_Value,perl = TRUE))

# 4. Question marks
cat("Fix leading question marks\n")
o<-grepl(pattern = "^\\?",x =  datatbl$Raw_Value,perl = TRUE)
cat("Items: ",length(o))
cat("Found: ",sum(o))

# Remove question mark symbol and set quality code to 400
datatbl$Raw_Value[o==TRUE]<-gsub(pattern = "^\\?", replacement = "", x = datatbl$Raw_Value[o==TRUE])
datatbl$QC[o==TRUE]<-400

# Check changes
# a<-subset(datatbl,grepl(pattern = "^\\?",x =  datatbl$QRaw_Value,perl = TRUE))


# 5. Weird Turb Result
p<-grepl(pattern = "NTU",x =  datatbl$Raw_Value,perl = TRUE)
cat("Fix weird turbidity result\n")
cat("Items: ",length(p))
cat("Found: ",sum(p))

# Remove question mark symbol and set quality code to 400
datatbl$Raw_Value[p==TRUE]<-"*"
datatbl$QC[p==TRUE]<-100

# 6. Trailing fullstops
#Finding strings ending with "." and remove
l<-grepl(pattern = "\\.$",x =  datatbl$Raw_Value,perl = TRUE)
cat("Remove trailing decimal points\n")
cat("Items: ",length(l))
cat("Found: ",sum(l))
datatbl$Raw_Value[l==TRUE]<-gsub(pattern = "\\.$", replacement = "", x = datatbl$Raw_Value[l==TRUE])
# a<-subset(datatbl,grepl(pattern = "\\+$",x =  datatbl$QRaw_Value,perl = TRUE))

# 7. Replacing NAs with "*"
#Finding  NA's
q<-is.na(x =  datatbl$Raw_Value)
cat("Replace NA with asterix\n")
cat("Items: ",length(q))
cat("Found: ",sum(q))
datatbl$Raw_Value[q==TRUE]<-"*"

# a<-subset(datatbl,grepl(pattern = "\\+$",x =  datatbl$QRaw_Value,perl = TRUE))


# Check changes
# a<-subset(datatbl,grepl(pattern = "NTU",x =  datatbl$QRaw_Value,perl = TRUE))

# 8. Replacing commas
# Two options here I think
#  a. Use a regular expression to find and replace
#  b. use as.numeric() function in the data write portion of this code

# Will use the regular expression to clean up the string on the principal that anything in the
# the string that isn't a number or a period of a value qualifier shouldn't be there.


# Will run this twice to ensure that all commas removed. The Global qualifier "/g" doesn't give desired result
r<-grepl(pattern = "\\,+",x =  datatbl$Raw_Value,perl = TRUE)
cat("Remove commma's - thousand separators\n")
cat("Items: ",length(r))
cat("Found: ",sum(r))
datatbl$Raw_Value[r==TRUE]<-gsub(pattern = "\\,+", replacement = "", x = datatbl$Raw_Value[r==TRUE])

r<-grepl(pattern = "\\,+",x =  datatbl$Raw_Value,perl = TRUE)
cat("Remove commma's - thousand separators\n")
cat("Items: ",length(r))
cat("Found: ",sum(r))
datatbl$Raw_Value[r==TRUE]<-gsub(pattern = "\\,+", replacement = "", x = datatbl$Raw_Value[r==TRUE])


# 9. Down-coding specific method IDs

# Synthetic TONS
# MethodIDS = TON-CAL-1, TON-1-1

#Finding strings
ton.cal.1<-grepl(pattern = "TON-CAL-1",x =  datatbl$QMethodID,perl = TRUE)
ton.1.1  <-grepl(pattern = "TON-1-1",x =  datatbl$QMethodID,perl = TRUE)
cat("Downcoding Synthetic TONs\n")
cat("Items: ",length(ton.cal.1))
cat("Found TON-CAL-1: ",sum(ton.cal.1))
cat("Found TON-1-1  : ",sum(ton.1.1 ))

datatbl$QC[ton.cal.1==TRUE]<-300
datatbl$QC[ton.1.1==TRUE]  <-300

# Conductivities

condstr<-c("\\*COND_US-1","\\*COND_US-6","\\*COND_US-4","\\*COND_US-5","\\*COND_US-7","\\*COND_US-3","\\*COND_US-8")
for(cc in 1:length(condstr)){
  conds<-grepl(pattern = condstr[cc],x =  datatbl$QMethodID,perl = TRUE)
  cat("Found",condstr[cc],": ",sum(conds),"\n")
  datatbl$QC[conds==TRUE]<-200
}



# determining dates to filter following methodIds by. Where date is less than 1-Jun-2010, then downcode
# following methodIDs

bb<- datatbl$mowsecs<mowSecs("01/06/2010")

otherstr <- c("\\*PH-6","\\*PH-11","\\*PH-13","\\*DO-6","\\*PH-14","\\*DO-7","\\*PH-12","\\*DO-5","\\*DO-8","\\*PH-16","DOSAT-2","\\*DO MG/L-2","\\*PH-20",
              "DOSAT-4","\\*DO MG/L-4","\\*PH-22","DOSAT-1","\\*DO MG/L-1","\\*PH-19","DOSAT-3","\\*DO MG/L-3","\\*PH-21","DOSAT-5","DOSAT-7","\\*DO MG/L-5",
              "\\*DO MG/L-8","DOSAT-6","\\*DO MG/L-7","DOSAT-8","\\*DO MG/L-6","\\*PH-23","DOSAT-9","\\*DO MG/L-9","\\*PH-24","DOSAT10","\\*DO MG/L-10",
              "\\*PH-25","\\*PH-17")

for(cc in 1:length(otherstr)){
  oths<-grepl(pattern = otherstr[cc],x =  datatbl$QMethodID,perl = TRUE)
  cat("Found",otherstr[cc],": ",sum(oths),"\n")
  ## intersecting logical vectors and updating Quality Codes
  datatbl$QC[oths & bb]<-200
}



# In addition Field pH (HRC)  >14 to 100 and Field DO Concentration >20 to 100
fieldPH <- (as.numeric(datatbl$Raw_Value) > 14) & (datatbl$HMeasurement=="Field pH (HRC)")
fieldDO <- (as.numeric(datatbl$Raw_Value) > 20) & (datatbl$HMeasurement=="Field DO Concentration (HRC)")

datatbl$QC[fieldPH==TRUE]<-100
datatbl$QC[fieldDO==TRUE]<-100

## DATA CLEANSING END   ##------------------------------------

# Sort dataset by SiteName, Measurement, Date and Raw_vALUE
# Make sure there is a trailing "," comma after the order() function, otherwise this will throw an error.
datatbl<-datatbl[order(datatbl$HSiteName,datatbl$HMeasurement,datatbl$mowsecs,datatbl$Raw_Value),]



# Sort dataset by SiteName, Measurement, Date and Project
#datatbl<-datatbl1[order(datatbl$HSiteName,datatbl$HMeasurement,datatbl$mowsecs,datatbl$Project)]

datatbl<-as.tbl(datatbl)
datatbl<-left_join(datatbl,SampleComm,by="SampleID")

#Test data assumptions on extract
#s<-subset(datatbl,HSiteName=="Mangatainoka at Brewery - S.H.2 Bridge" & HMeasurement=="Field Temperature (HRC)")

#Finding values with leading ">" symbol
#s<-subset(datatbl,grepl(pattern = "^\\*",x =  datatbl$Raw_Value,perl = TRUE))


#Use this line during testing only
#datatbl$mowsecs   <- datatbl$Qmowsecs

## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "Horizons")
#saveXML(con$value(), file="out.xml")

tab <- "\t"

max<-length(datatbl$Date)

i<-1
#for each site
while(i<=max){
  s<-datatbl$HSiteName[i]
  # store first counter going into while loop to use later in writing out sample values
  start<-i
  
  cat(i,datatbl$HSiteName[i],"\n")   ### Monitoring progress as code runs
  
  while(datatbl$HSiteName[i]==s){
    #for each measurement
    #cat(datatbl$SiteName[i],"\n")
    con$addTag("Measurement",  attrs=c(SiteName=datatbl$HSiteName[i]), close=FALSE)
    
    #### I need to join in the DatasourceName to the Measurement name here, or perhaps in the qetl CSV
    con$addTag("DataSource",  attrs=c(Name=datatbl$HDatasource[i],NumItems="2"), close=FALSE)
    con$addTag("TSType", "StdSeries")
    con$addTag("DataType", "WQData")
    con$addTag("Interpolation", "Discrete")
    con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
    con$addTag("ItemName", datatbl$HMeasurement[i])
    con$addTag("ItemFormat", "F")
    con$addTag("Divisor", "1")
    con$addTag("Units", datatbl$HUnits[i])
    #con$addTag("Units", "Joking")
    con$addTag("Format", datatbl$HFormat[i])
    con$closeTag() # ItemInfo
    con$closeTag() # DataSource
    #saveXML(con$value(), file="out.xml")
    
    # for the TVP and associated measurement water quality parameters
    con$addTag("Data", attrs=c(DateFormat="mowsecs", NumItems="2"),close=FALSE)
    d<- datatbl$HMeasurement[i]
    
    cat("       - ",datatbl$HMeasurement[i],"\n")   ### Monitoring progress as code runs
    
    # setting up value to count up duplicate samples for given measurement so that this can be added to sample time.
    correct <- 0
    
    # setting up condition for skiping current value if sample duplicated across multiple project names
    skipCurrent <- FALSE
    
    while(datatbl$HMeasurement[i]==d){
      
      # remember mowsec (record mowsec at end of while loop) mowsec <- datatbl$mowsecs[i].
      # If next sample has the same mowsec AND the same project name 
      #       then increment mowsec by 1 AND write new result
      # Else if next sample has the same mowsec and a differenct project name
      #       then append the project name to the previous sample AND Skip to next record
      
      
      # Skipping mowsec and project check for first time through while loop...
      if(i!=start){
        if(datatbl$mowsecs[i]==datatbl$Qmowsecs[i-1]){
          ## If project the same, assume duplicate samples collected. Increment time by 1 second for each one.
          cat("          - duplicate sample time: ", datatbl$Qmowsecs[i-1], " ",datatbl$mowsecs[i], "\n")
          
          if(datatbl$SampleID[i]!=datatbl$SampleID[i-1]){
            #Where sampleID is different, treat as independent sample at that date/time - add 1 second to time.
            datatbl$mowsecs[i] <- datatbl$mowsecs[i-1] + 1
            
          } else if(datatbl$Project[i]==datatbl$Project[i-1]){        
            #where project name is the same for the date/time, treat as independent sample at that date/time - add 1 second to time.
            datatbl$mowsecs[i] <- datatbl$mowsecs[i-1] + 1
            
            
          } else if(datatbl$QMethodID[i]!=datatbl$QMethodID[i-1]){        
            #where QMethodID are different for the date/time, treat as independent sample at that date/time - add 1 second to time.
            datatbl$mowsecs[i] <- datatbl$mowsecs[i-1] + 1
            
          } else {
            #In all other cases, skip this sample as inferred to be a duplicate record generated by QETL extract from QUALARC.
            skipCurrent<-TRUE
          }
        } else{
          #correct <- 0
          ## finish of mowsec comparison
        }
      } ## finish of counter check
      
      
      if(!skipCurrent){
        # for each tvp
        # Handle Greater than symbols
        if(grepl(pattern = "^>",x =  datatbl$Raw_Value[i],perl = TRUE)){
          con$addTag("E",close=FALSE)
          con$addTag("T",datatbl$mowsecs[i])
          con$addTag("I1", gsub(pattern = "^>", replacement = "", x = datatbl$Raw_Value[i]))
          con$addTag("I2", paste("$ND",tab,">",tab,
                                 "Method",tab,datatbl$QMethodDescription[i],tab,
                                 "Detection Limit",tab,datatbl$Qdetection[i],tab,
                                 "Lab",tab,datatbl$Qlab[i],tab,
                                 "$QC",tab,datatbl$QC[i],tab,
                                 "Result Value",tab,datatbl$Raw_Value[i],tab,sep=""))
          con$closeTag() # E
          
          # Handle Less than symbols  
        } else if(grepl(pattern = "^<",x =  datatbl$Raw_Value[i],perl = TRUE)){
          con$addTag("E",close=FALSE)
          con$addTag("T",datatbl$mowsecs[i])
          con$addTag("I1", gsub(pattern = "^<", replacement = "", x = datatbl$Raw_Value[i]))
          con$addTag("I2", paste("$ND",tab,"<",tab,
                                 "Method",tab,datatbl$QMethodDescription[i],tab,
                                 "Detection Limit",tab,datatbl$Qdetection[i],tab,
                                 "Lab",tab,datatbl$Qlab[i],tab,
                                 "$QC",tab,datatbl$QC[i],tab,
                                 "Result Value",tab,datatbl$Raw_Value[i],tab,sep=""))
          con$closeTag() # E
          
          # Handle Asterixes  
        } else if(grepl(pattern = "^\\*",x =  datatbl$Raw_Value[i],perl = TRUE)){
          con$addTag("E",close=FALSE)
          con$addTag("T",datatbl$mowsecs[i])
          con$addTag("I1", gsub(pattern = "^\\*", replacement = "0", x = datatbl$Raw_Value[i]))
          con$addTag("I2", paste("$ND",tab,"*",tab,
                                 "Method",tab,datatbl$QMethodDescription[i],tab,
                                 "Detection Limit",tab,datatbl$Qdetection[i],tab,
                                 "Lab",tab,datatbl$Qlab[i],tab,
                                 "$QC",tab,datatbl$QC[i],tab,
                                 "Result Value",tab,datatbl$Raw_Value[i],tab,sep=""))
          con$closeTag() # E
          
          # Write all other result values  
        } else {
          con$addTag("E",close=FALSE)
          con$addTag("T",datatbl$mowsecs[i])
          con$addTag("I1", datatbl$Raw_Value[i])
          con$addTag("I2", paste("Method",tab,datatbl$QMethodDescription[i],tab,
                                 "Detection Limit",tab,datatbl$Qdetection[i],tab,
                                 "Lab",tab,datatbl$Qlab[i],tab,
                                 "$QC",tab,datatbl$QC[i],tab,
                                 "Result Value",tab,datatbl$Raw_Value[i],tab,sep=""))
          con$closeTag() # E
        }
      }
      skipCurrent<-FALSE # reseting condition
      #correct<-0
      i<-i+1 # incrementing overall for loop counter
      if(i>max){break}
      
      
    }
    # next
    con$closeTag() # Data
    con$closeTag() # Measurement
    
    
    if(i>max){break}
    # Next 
  }
  # store last counter going out of while loop to use later in writing out sample values
  end<-i-1
  
  # Adding WQ Sample Datasource to finish off this Site
  # along with Sample parameters
  con$addTag("Measurement",  attrs=c(SiteName=datatbl$HSiteName[start]), close=FALSE)
  con$addTag("DataSource",  attrs=c(Name="WQ Sample", NumItems="1"), close=FALSE)
  con$addTag("TSType", "StdSeries")
  con$addTag("DataType", "WQSample")
  con$addTag("Interpolation", "Discrete")
  con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
  con$addTag("ItemName", "WQ Sample")
  con$addTag("ItemFormat", "S")
  con$addTag("Divisor", "1")
  con$addTag("Units")
  con$addTag("Format", "$$$")
  con$closeTag() # ItemInfo
  con$closeTag() # DataSource
  
  # for the TVP and associated measurement water quality parameters
  con$addTag("Data", attrs=c(DateFormat="mowsecs", NumItems="1"),close=FALSE)
  # for each tvp
  ## THIS NEEDS SOME WORK.....
  ## just pulling out SampleID, ProjectName and mowsecs
  sample<-datatbl[c(2,16,18,23:25)] ## [c(SampleID, Project, mowsecs, sampleProcedure,collectedBy,comments)]
  sample<-sample[start:end,]
  sample<-distinct(sample,mowsecs,Project)
  ## Deal with any duplicate samples across two or more projects
  ## The projects need to be concatenated into the first sample of the duplicate group
  
  ## Could use tidyr::spread followed by tidyr::unite to deal with duplicates
  ## however, it is not appropriate to the type of data in this dataframe
  
  ## Will take a brute force approach through a for loop to concatenate project where
  ## duplicated. Will need store original project column at the start
  ## just in case something goes awry.
  sample$QProject <- sample$Project
  #sample$Project <- sample$QProject
  zz<-nrow(sample)-1  #  omitting last row as processing done through to n-1
  if(zz>0){
    for(z in 1:zz){ 
      ## If sample the same
      #cat(sample$mowsecs[z],sample$mowsecs[z+1],"\n")
      
      ## Need to convert time to numeric to evaluate comparison here
      if(as.numeric(sample$mowsecs[z])==as.numeric(sample$mowsecs[z+1])){
        #cat("Matched DATETIME\n")
        #cat(paste(sample$Project[z],sample$Project[z+1],sep=" ## "),"\n")
        ## if project different
        if(sample$Project[z]!=sample$Project[z+1]){
          #cat("Join Project Names\n")
          # Concatenate project values
          sample$Project[z] <- paste(sample$Project[z],sample$Project[z+1],sep="|")
          sample$Project[z+1] <- sample$Project[z]
          #cat(sample$Project[z],"\n")
        }
      } else{
        #cat("No match\n")
      }
      
    }
  }
  
  ## Handling duplicated samples and project associations.
  if(zz>1){
    for(z in 2:zz+1){ 
      if(sample$SampleID[z]==sample$SampleID[z-1]){
        if((as.numeric(sample$mowsecs[z])-as.numeric(sample$mowsecs[z-1]))==1){
          sample$Project[z]<-sample$Project[z-1]
        }
      }
    }
  }
  
  ## selecting distinct records to write wq sample data
  sample<-distinct(sample,mowsecs)
  sample<-sample[order(sample$mowsecs),]
  ## THIS NEEDS SOME WORK.....
  for(a in 1:nrow(sample)){ 
    con$addTag("E",close=FALSE)
    con$addTag("T",sample$mowsecs[a])
    con$addTag("I1", paste("Sample ID",tab,sample$SampleID[a],tab,
                           "Project",tab,sample$Project[a],tab,
                           "Sampling Method",tab,sample$sampleProcedure[a],tab,
                           "SampledBy",tab,sample$collectedBy[a],tab,
                           "Comments",tab,sample$comments[a],tab,sep=""))
    con$closeTag() # E
  }
  
  con$closeTag() # Data
  con$closeTag() # Measurement    
  
}
cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file="qetl200.xml")
cat("Finished",Sys.time()-tm,"\n")
