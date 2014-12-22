## R Programming Assignment 2
## These functions collectively create a special "matrix" object that can
## cache its inverse, and a function that computes the inverse of the special
## matrix. If the inverse has already been computed, the cached inverse is 
## returned.
## For the puposes of this exercise it is assumed that the input matrix is
## always invertible.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Compute the inverse of the special "matrix" as returned by makeCacheMatrix.
## If the inverse has already been computed, return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
}
