corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    source('complete.R')
    
    files <-complete(directory,id =1:332)
    x <- thFilter(files,threshold,2)
    
    vCor <- c()
    for (each in x[,1]) {
        
        corValue <- compCor(directory,each)
        vCor <- append(vCor,corValue)
    }
    vCor
}

thFilter <- function(file, threshold, col) {
    vect <- file[,col]
    tFilter <- vect > threshold
    ids <- file[,1][tFilter]
    noms <- file[,2][tFilter]
    result <-data.frame(ids,noms)
    result
}

compCor <- function(directory,id) {
    fPath <- get_file_path(directory,id)
    file <- read.csv(fPath)
    filter <- (!is.na(file[,2])) & (!is.na(file[,3]))
    vSulfate <- file[,"sulfate"][filter]
    vNitrate <- file[,"nitrate"][filter]
    corrolation <- cor(vSulfate,vNitrate)
    corrolation
}