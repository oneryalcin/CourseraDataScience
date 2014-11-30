best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    #Choose the state
    stateDF<- byState[[state]]
    
    #Convert the string to numbers for that disease
    stateDF[,pr] <- as.numeric(stateDF[,pr])
    
    #Sort by disease from min to max
    sortedDF <- stateDF[order(stateDF[,pr]),]
    print(sortedDF[1,2])    
}


#dd[order(dd$y),]
#byState <- split(hospData,hospData$State)
#texas <- byState$TX