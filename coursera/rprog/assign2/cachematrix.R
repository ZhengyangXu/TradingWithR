## This file contains two functions, that when used together, will allow the
## user to save cpu time by caching the inverse of a given matrix. If asked to
## compute the inverse again, instead of spending cycles computing the inverse,
## the code will pull from the cache instead.

## makeCacheMatrix takes a matrix as input and returns a list object in its own
## environment which has the ability to store the original matrix as well as its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes the list function returned from makeCacheMatrix and returns
## the inverse matrix, pulling from cache if the matrix hasn't changed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}