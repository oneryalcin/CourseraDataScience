#a<-c(1,NA,2,NA,3,NA,5,6,NA,NA,NA,7,8,9)
#matrix(a,7,2)

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    if (id[1] != sort(id)[1]) {
        id <- sort(id)
    }    
    
    nobs<-c()
    paths<-get_file_path(directory,id)
    for (each in paths) {
        file<-read.csv(each)
        nob<-filterNA(file,2,3)
        nobs<-append(nobs,nob)
    }
    #print(id)
    #print(nobs)
    res<-data.frame(id,nobs)
    res
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

filterNA <-function(file,col1,col2) {
    col1Filter<-!is.na(file[,col1])
    col2Filter<-!is.na(file[,col2])
    unifiedFilter<-col1Filter & col2Filter
    result <- sum(as.numeric(unifiedFilter))
    result
}
