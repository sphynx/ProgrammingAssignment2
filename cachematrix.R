# Certain matrix operations may be costly (for example, matrix
# inversion), so in order not to re-calculate them every time, we can
# cache the results and reuse them later if needed. This file contains
# functions for caching matrix inversion operation expressed via
# `solve` function.

# Example of functions usage follows. It demonstrates the profit of
# caching. Here we are inverting a 1000x1000 matrix of uniformly
# distributed random numbers from 0 to 1. Calculating it for the first
# time takes around 0.23s on my machine. Getting it from the cache is
# almost instant.

## > cm <- makeCacheMatrix(matrix(runif(1e6), 1000, 1000))
## > system.time(x <- cacheSolve(cm))
## recalculating inverse
##    user  system elapsed
##   0.231   0.006   0.106
## > system.time(x <- cacheSolve(cm))
## getting cached inverse
##    user  system elapsed
##       0       0       0
## > system.time(x <- cacheSolve(cm))
## getting cached inverse
##    user  system elapsed
##   0.001   0.000   0.001

## This function acts as a constructor of "smart" matrices which can
## cache its inverse calculations.
makeCacheMatrix <- function(x = matrix()) {
    # Private (a-la private field in OOP).
    inverse <- NULL

    # Functions (a-la methods in OOP).
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inv) {
        inverse <<- inv
    }

    getInverse <- function() {
        inverse
    }

    # Exposed API.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}

## This function actually calculates and caches the "solve" function,
## which returns the inverse of the matrix. Accepts a "smart" matrix
## created by "makeCacheMatrix" as an argument.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()

    # Calculate if needed, otherwise just use what's cached.
    if (is.null(inv)) {
        message("recalculating inverse")
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
    } else {
        message("getting cached inverse")
    }

    inv
}
