## ----------------------------------------------------------------------------
## Get first line of each CSV file found recursing through filepath and file
##
## Purpose: to check that column headers are same for all files 
##  (done by eyeballing in Excel)
## ----------------------------------------------------------------------------

od <- getwd()
#filepath = "//ares/Environmental Monitoring Programmes/Discrete WQ Laboratory Files/Discrete WQ - Laboratory Results (CSV)/ELS Results (CSV) Quaterly batches"
filepath = "//file/scanner/Sean/WaterCare"
setwd(filepath)

files <- list.files(path = filepath, pattern="*.csv",recursive = TRUE)
for(i in 1:length(files)){
    f <- paste(filepath,files[i],sep="/")

    a<-readLines(f, n = 1) 

    cat(a,"\n")
    cat(paste(filepath,files[i],a,sep=","),file = "csv_headers.txt",append=TRUE,sep="\n")
  
}
rm("filepath","files","f")

setwd(od)