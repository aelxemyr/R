best <- function(state, outcome) {
    ## Read outcome data
    outcome <- tolower(outcome)
    data <- read.csv("outcome-of-care-measures.csv", 
                     na.strings = "Not Available",
                     stringsAsFactors = F)
    
    ## Check that state and outcome are valid
    if (state %in% data[,7])
        data <- data[data[,7] == state,]
    else
        stop("invalid state")
    if (outcome == "heart attack")
        ## Heart attack mortality rate is column 11
        data <- data[,c(2,11)]
    else if (outcome == "heart failure")
        ## Heart failure mortality rate is column 17
        data <- data[,c(2,17)]
    else if (outcome == "pneumonia")
        ## Pneumonia mortality rate is column 23
        data <- data[,c(2,23)]
    else
        stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death rate
    names(data) <- c("Hospital", "Rate")
    data <- na.omit(data)
    data[data$Rate == min(data$Rate),]
}