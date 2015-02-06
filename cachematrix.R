##-------|---------|---------|---------|---------|---------|---------|---------|
## R Programming February 2015 Assignment 2
##
## This assignment consists of two functions to create a special "matrix" and
## compute and cache the inverse of the matrix so that the expensive inverse
## calculation only needs to be done once (until the matrix changes)
##
## Wayne Carriker
## 2015-02-05

## These routines are very rudimentary and provide no error checking to
## validate that there is an inverse for the specified matrix

## makeCacheMatrix
## This function creates a "smart" matrix which is really a list of functions
## to set the value of the matrix, get the value of the matrix, set the inverse
## of the matrix, and get the inverse of the matrix. The initial value of the
## matrix can be set in makeCacheMatrix (the defaulh is an empty matrix.)
## The get() and set(x) functions can be called directly to retrieve or change
## the value of the matrix, but the getinv() and setinv(i) functions should
## never be called directly.
makeCacheMatrix <- function(x = matrix()) {
    # This code is nearly identical to the sample "makeVector" code
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve  
## This functions solves for the inverse of the "smart" matrix, simply returning
## the cached value of the inverse if it has already been calculated for the
## current value of the "smart" matrix, or calculating the inverse, caching it,
## and returning it if it has not been previously calculated
cacheSolve <- function(x, ...) {
    # This code is simplified from the sample "cachemean" code as there is no
    # need to display a message when the cache is used
    i <- x$getinv()
    if (is.null(i)) {
        data <- x$get()
        i <- solve(data)
        x$setinv(i)        
    }
    i
}

## Example
## cm <- makeCacheMatrix(matrix(1:4,2,2))
## im <- cacheSolve(cm)
## cm$get() %*% im
## Should produce
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
