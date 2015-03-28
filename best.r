## this function finds the hospital in a given state with the
## lowest rate of 30-day mortality for a given outcome

best <- function(state, outcome) {
    #load data and vars
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
    
    #slice data
    dataForState <- subset(outcomeData, State == state)
    dataForState[,outcomeCol] <- as.numeric(dataForState[,outcomeCol])
    candidates <- dataForState[which(dataForState[,outcomeCol] == min(dataForState[,outcomeCol], na.rm=TRUE)), ]
    #sort candidates
    sort.candidates <- with(candidates,  candidates[order(Hospital.Name) , ])
    #return first hospital from sorted set
    candidates[1,2]
}