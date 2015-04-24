corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    source('complete.R')
    c = complete(directory)
    filtered = subset(c, c[,2] > threshold, id)
    filtered = as.numeric(filtered[,1])

    if (length(filtered) == 0) {
        return(vector())
    }
    else {
        result = NULL
        for (id in filtered) {
            t      = read.csv(paste(directory, '/', sprintf('%03d', id), '.csv', sep=''))
            t      = na.omit(t)
            corval = cor(t$sulfate, t$nitrate)
            result = c(result, corval)
        }
        #as.numeric(sprintf('%.5f', result))
        as.numeric(result)
    }
}