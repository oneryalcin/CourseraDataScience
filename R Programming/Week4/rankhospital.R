rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    hospData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    byState <- split(hospData,hospData$State)   
    ## Check that state and outcome are valid
    stateList <- unique(hospData$State)
    diseaseList <-c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% stateList)) stop('invalid state')
    if (!(outcome %in% diseaseList)) stop('invalid outcome')
    
    if (outcome == "heart attack") pr <- 11
    else if (outcome == "heart failure") pr <- 17
    else pr <-23
    
    wrstFlag<- FALSE
    if (num == "best") num <-1
    if (num == "worst") {
        num <- 1
        wrstFlag <- TRUE
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #Choose the state
    stateDF<- byState[[state]]
    
    #Convert the string to numbers for that disease
    stateDF[,pr] <- as.numeric(stateDF[,pr])
    
    #Sort by disease from min to max
    if (wrstFlag==TRUE) sortedDF <- stateDF[order( stateDF[,pr], stateDF[,2],decreasing = TRUE),]
    else sortedDF <- stateDF[order(stateDF[,pr],stateDF[,2]),]
    
    print(sortedDF[num,2]) 
}