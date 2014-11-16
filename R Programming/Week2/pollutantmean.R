pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    if (pollutant =="sulfate")
        selectcolumn=2
    else
        selectcolumn=3
    
    ## Get the files
    paths = get_file_path(directory,id)
    
    ## Read each selected csv file
    totalSum<-0
    totalReadings<-0
    for (each in paths) {
        file<-read.csv(each)
        readings <- get_readings(file,selectcolumn)
        totalSum <- totalSum+readings[1]
        totalReadings <- totalReadings + readings[2]
    }
    totalMean<-totalSum/totalReadings
    totalMean
}

get_file_path<- function(directory,id,end=332) {
    ## This function retuns the path of the csv files in given id and directory
    ## > get_files("specdata",9:10)
    ##[1] "specdata/009.csv" "specdata/010.csv"
    
    ch<-character(end)
    ch[id] <- paste(directory,"/",sprintf("%03d",id),".csv",sep="")
    sel<- ch[1:end] !=""
    ch[sel]   
}


get_readings<- function(file,column) {
    readings<-file[,column][!is.na(file[,column])]
    sum_totalReadings=numeric(2)
    sum_totalReadings[1]=sum(readings)
    sum_totalReadings[2]=length(readings)
    sum_totalReadings
}

