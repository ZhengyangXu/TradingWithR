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
    zs = function(x) sprintf('%03d', x)
    mdf = NULL
    for (i in id) {
        t = read.csv(paste(directory, '/', zs(i), '.csv', sep=''))
        n = length(complete.cases(t)[complete.cases(t) == T])
        mdf = rbind(mdf, c(i, n))
    }
    mdf = as.data.frame(mdf)
    colnames(mdf) = c('id', 'nobs')
    mdf
}