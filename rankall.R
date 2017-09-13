rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome <- tolower(outcome)
    data <- read.csv("outcome-of-care-measures.csv", 
                     na.strings = "Not Available",
                     stringsAsFactors = F)
    
    ## Check that outcome is valid
    if (outcome == "heart attack")
        ## Heart attack mortality rate is column 11
        data <- data[,c(2,7,11)]
    else if (outcome == "heart failure")
        ## Heart failure mortality rate is column 17
        data <- data[,c(2,7,17)]
    else if (outcome == "pneumonia")
        ## Pneumonia mortality rate is column 23
        data <- data[,c(2,7,23)]
    else
        stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank and return a data
    ## frame with the hospital names and the (abbreviated) state name
    names(data) <- c("Hospital", "State", "Rate")
    # Remove NAs
    data <- na.omit(data)
    # Order data first by mortality rate, then by hospital name
    data <- data[order(data$Rate, data$Hospital),]
    # Ensure num has a numerical value
    num <- if (num == "best") 1 else num
    # Split data by state and find the hospital of the given rank
    data <- lapply(split(data, data$State), function(x) x$Hospital[num])
    
    data.frame(Hospital = unlist(data),
               State = names(data))
}