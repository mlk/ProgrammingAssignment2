## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that supports caching of the matrixes inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 
cacheSolve <- function(x, ...) {
    cacheSolveWith(solve, x, ...)
}

## This version of the code allows for the function which performs
## the "solve" to be injected (see Dependency Injection)
cacheSolveWith <- function(iSolve, x, ...) {
    if (is.null(x$getinverse())) {
        value <- iSolve(x$get(), ...)
        x$setinverse(value)
    }
    x$getinverse()
}