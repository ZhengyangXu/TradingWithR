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
    mdf = NULL
    for (i in id) {
        t = read.csv(paste(directory, '/', sprintf('%03d', i), '.csv', sep=''))
        n = sum(complete.cases(t))
        mdf = rbind(mdf, c(i, n))
    }
    mdf = as.data.frame(mdf)
    colnames(mdf) = c('id', 'nobs')
    mdf
}