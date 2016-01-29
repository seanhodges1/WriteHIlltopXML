## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
mowSecs <- function(x){
  s<-strptime("1940-01-01","%Y-%m-%d")
  t<-strptime(x,"%Y-%m-%d %H:%M:%S")
  t<-strptime(x,"%Y-%m-%d %H:%M:%S")
  x<-(t-s)*86400
}


## Load libraries ------------------------------------------------
require(RODBC)   ### ODBC library for SQL connection
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(XML)     ### XML library to write hilltop XML

## Load Hilltop tables to dataframes -----------------------------

# SQL connection - OPEN
conn <- odbcDriverConnect('driver={SQL Server};server=hilltopdb;database=Hilltop;trusted_connection=true')

# tables to dataframes
labTests         <- sqlQuery(conn, 
                             paste("SELECT [LabTestID],[TestID],[LabName],[LabMethod],[LabTestName],[Units],[DetectionLimit]",
                                   "FROM [hilltop].[dbo].[LabTests] LEFT JOIN [hilltop].[dbo].[Labs]",
                                   "ON [hilltop].[dbo].[LabTests].[LabID] = [hilltop].[dbo].[Labs].[LabID]"),
                             stringsAsFactors=FALSE)

Tests            <- sqlQuery(conn, 
                             paste("SELECT [TestID],[TestName],[HilltopMeasurementID]",
                                   "FROM [hilltop].[dbo].[Tests]"),
                             stringsAsFactors=FALSE)

Measurements     <- sqlQuery(conn, 
                             paste("SELECT [MeasurementID],[MeasurementName],[DataSourceName],[Units],[Format]",
                                   "FROM [hilltop].[dbo].[Measurements]"),
                             stringsAsFactors=FALSE)


Datasources      <- sqlQuery(conn, 
                             paste("SELECT [DataSourceID],[DataSourceName],[NumberOfItems],[InterpolationMethod],[DataType]",
                                   "FROM [hilltop].[dbo].[Datasources]"),
                             stringsAsFactors=FALSE)

SampleSites     <- sqlQuery(conn, 
                            paste("SELECT [SampleID],[Hilltop].[dbo].[Samples].[SiteID],[SiteName],[SampleTime]",
                                  "FROM [hilltop].[dbo].[Samples] LEFT JOIN [hilltop].[dbo].[Sites]",
                                  "ON [hilltop].[dbo].[Samples].[SiteID]=[hilltop].[dbo].[Sites].[SiteID]",
                                  "WHERE [StatusID]=5"),
                            stringsAsFactors=FALSE)

SampleProjects  <- sqlQuery(conn, 
                             paste("SELECT  s2.SampleID,p.ProjectName",
                                   "FROM [hilltop].[dbo].[Samples] as s2 LEFT JOIN [hilltop].[dbo].[Projects] as p",
                                   "ON s2.ProjectID=p.ProjectID"),
                             stringsAsFactors=FALSE)



# SQL connection - CLOSE
close(conn)
rm(conn)

# Joining tests data to form one Table (could just do this through single SQL query)
TestsTable<-left_join(labTests,Tests,by="TestID")
TestsTable$MeasurementID<-TestsTable$HilltopMeasurementID
TestsTable<-left_join(TestsTable,Measurements,by="MeasurementID")
TestsTable<-left_join(TestsTable,Datasources,by="DataSourceName")

rm("Datasources","Measurements","Tests","labTests")


# Joining csv dataFrame "data" from "LoadCSVsToDataTable.R" to SampleSites View from sQL
dataset<-left_join(data,SampleSites,by="SampleID")

# Optional output of single CSV of all data with Join to SampleSites table - useful for reviewing data
# Takes about 5 minutes to complete writing file.
# write.csv(data,"//file/herman/O/IT/06/02/SourceCode/RScripts/LaboratoryCSVs/watercareSampleSitesData.csv")



# Pruning dataset back to useable cases
dataset<-dataset[complete.cases(dataset$SampleID),]
dataset<-dataset[complete.cases(dataset$SiteID),]

# Join test and method information to Sample table
# this will set up data for writing to xML
dataset$LabTestName<-dataset$tcode   ## Create Join field with the correct name
dataset<-left_join(dataset,TestsTable,by="LabTestName")
dataset<-left_join(dataset,SampleProjects,by="SampleID")
dataset<-dataset[complete.cases(dataset$MeasurementName),]

#Fixing final data issues
dataset$ltgt[is.na(dataset$ltgt)] <- ""
dataset$mowsecs<-mowSecs(dataset$SampleTime)

# Sort dataset by SiteName and Date and BatchNo
dataset<-dataset[order(dataset$SiteName,dataset$MeasurementName,dataset$SampleTime,dataset$batchno,dataset$filetime),]
dataset<-dataset[complete.cases(dataset$MeasurementName),]


# Select the last value for each group
# using dplyr
# Creating a table that can be grouped
datatable<-as.tbl(dataset)
# Grouping table by Site, Measurement and SampleDate
datatbl<-group_by(datatable,as.factor(dataset$SiteName),as.factor(dataset$MeasurementName),as.factor(dataset$SampleTime))
# Returning datatable with the newest filetime associated with the csv file for each group
# First remove those fields that may cause ambuiguity due to varied date representations for the same date
datatbl<-top_n(datatbl,1,filetime)
datatbl<-distinct(datatbl)
datatbl<-datatbl[complete.cases(datatbl$MeasurementName),]

#Test data assumptions on extract
s<-subset(datatbl,SiteName=="Whanganui at Pipiriki" & MeasurementName=="Nitrate (HRC)")
s<-subset(datatbl,SiteName=="Woodville STP at Secondary oxpond waste")


## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "Horizons")
#saveXML(con$value(), file="out.xml")


max<-nrows(datatbl)
i<-1
#for each site
while(i<=max){
    s<-datatbl$SiteName[i]
    # store first counter going into while loop to use later in writing out sample values
    start<-i
    
    cat(i,datatbl$SiteName[i],"\n")   ### Monitoring progress as code runs
    
    while(datatbl$SiteName[i]==s){
        #for each measurement
        #cat(datatbl$SiteName[i],"\n")
        con$addTag("Measurement",  attrs=c(SiteName=datatbl$SiteName[i]), close=FALSE)
        con$addTag("DataSource",  attrs=c(Name=datatbl$DataSourceName[i],NumItems=datatbl$NumberOfItems[i]), close=FALSE)
        con$addTag("TSType", "StdSeries")
        con$addTag("DataType", "WQData")
        con$addTag("Interpolation", "Discrete")
        con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
        con$addTag("ItemName", datatbl$MeasurementName[i])
        con$addTag("ItemFormat", "F")
        con$addTag("Divisor", "1")
        con$addTag("Units", datatbl$Units.y[i])
        #con$addTag("Units", "Joking")
        con$addTag("Format", datatbl$Format[i])
        con$closeTag() # ItemInfo
        con$closeTag() # DataSource
        #saveXML(con$value(), file="out.xml")
        
        # for the TVP and associated measurement water quality parameters
        con$addTag("Data", attrs=c(DateFormat="mowsecs", NumItems="2"),close=FALSE)
        d<- datatbl$MeasurementName[i]
        
        cat("       - ",datatbl$MeasurementName[i],"\n")   ### Monitoring progress as code runs
        
        while(datatbl$MeasurementName[i]==d){
            # for each tvp
            con$addTag("E",close=FALSE)
            con$addTag("T",datatbl$mowsecs[i])
            con$addTag("I1", trim(paste(datatbl$ltgt[i],datatbl$value[i],sep="")))
            con$addTag("I2", paste("BatchNo\t",datatbl$batchno[i],"\t",
                                   "Method\t",datatbl$LabMethod[i],"\t",
                                   "Lab\t",datatbl$LabName[i],"\t",
                                   "Detection Limit\t",datatbl$detectionLimit[i],"\t",
                                   "Cost\t",datatbl$Cost[i],"\t",
                                   "Lower Confidence Limit\t",datatbl$lowerCI[i],"\t",
                                   "Upper Confidence Limit\t",datatbl$upperCI[i],"\t",
                                   "Uncertainty\t",datatbl$uncertainty[i],"\t",
                                   "Result Value\t",datatbl$result[i],"\t",sep=""))
            
            con$closeTag() # E
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
    con$addTag("Measurement",  attrs=c(SiteName=datatbl$SiteName[start]), close=FALSE)
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
    sample<-datatbl[c(2,42,43)]
    sample<-sample[start:end,]
    sample<-distinct(sample,mowsecs)
    sample<-sample[order(sample$mowsecs),]
    ## THIS NEEDS SOME WORK.....
    for(a in 1:nrow(sample)){ 
        con$addTag("E",close=FALSE)
        con$addTag("T",sample$mowsecs[a])
        con$addTag("I1", paste("Sample ID\t",sample$SampleID[a],"\t",
                               "Project\t",sample$ProjectName[a],"\t",sep=""))
        con$closeTag() # E
    }

    con$closeTag() # Data
    con$closeTag() # Measurement    
 
}
cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file="out-20150804.xml")
cat("Finished",Sys.time()-tm,"\n")
