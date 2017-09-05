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
    
    getCSVList <- function(id) {
        lapply(dir(directory, full.names=T)[id], read.csv)
    }
    
    getnobs <- function(data) {
        nrow(data[complete.cases(data),])
    }
    
    data.frame(id = id, nobs = sapply(getCSVList(id), getnobs))
}