rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    validOutcomes = vector(mode="list", length=3)
    names(validOutcomes) <- c("heart attack", "heart failure", "pneumonia")
    validOutcomes[1] <- 11; validOutcomes[2] <- 17; validOutcomes[3] <- 23
    outcomeCol <- validOutcomes[[outcome]]
    
    #error checking
    if(!state %in% outcomeData$State)
        stop("invalid state")
    if(!outcome %in% names(validOutcomes))
        stop("invalid outcome.\noutcomes can be \"heart attack\", \"heart failure\", or \"pneumonia\"")
    
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}