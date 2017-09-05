corr <- function(directory, threshold = 0) {
    ## 'directory is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observatons (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    corSulfateNitrate <- function(data) {
        completeData <- data[complete.cases(data),]
        cor(completeData[,"sulfate"], completeData[,"nitrate"])
    }
    
    source("complete.R")
    cc <- complete(directory)
    idList <- cc[cc$nobs > threshold,][,"id"]
    dataList <- lapply(dir(directory, full.names=T)[idList], read.csv)
    sapply(dataList, corSulfateNitrate)
}
