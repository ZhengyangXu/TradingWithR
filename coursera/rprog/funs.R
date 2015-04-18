add2 = function(x, y) {
    x+y
}

above = function(x, y = 10) {
    use = x > y
    x[use]
}

columnmean = function(y, removeNAs = TRUE) {
    nc = ncol(y)
    means = numeric(nc)
    for (i in 1:nc) {
        means[i] = mean(y[,i], na.rm=removeNAs)
    }
    means
}