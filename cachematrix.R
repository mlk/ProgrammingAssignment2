## This package supports a cachable matrix. 


#' Creates a matrix that supports caching of the matrixes inverse.
#' @param x The matrix which will support caching.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    #' @param y the  Matrix.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #' @return The matrix
    get <- function() x
    
    #' @param i the inverse of the matrix. Do not use directly, instead use \code{cacheSolve}.
    setinverse <- function(i) inverse <<- i
    #' @return the inverse of the matrix. Do not use directly, instead use \code{cacheSolve}.
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#' The cacheSolve function takes a cachable matrix and calls "solve" on it.
#' The solve is cached so future calls on it will be quick.
#' @param x A cachable Matrix (see makeCacheMatrix())
#' @param ... Any other paramiters to be sent to "Solve". Note: The cache method does not cache based on these params.
#' @return The inverse/solved matrix. 
cacheSolve <- function(x, ...) {
    cacheSolveWith(solve, x, ...)
}

#' This version of the code allows for the function which performs
#' the "solve" to be injected. This is to allow for testing.
#' @param iSolve The injected method used to provide the "solve" functionality.
#' @param x The cachable Matrix to "solve".
#' @param ... Any other paramiters to be sent to "Solve". Note: The cache method does not cache based on these params.
#' @return The inverse/solved matrix. 
cacheSolveWith <- function(iSolve, x, ...) {
    if (is.null(x$getinverse())) {
        value <- iSolve(x$get(), ...)
        x$setinverse(value)
    }
    x$getinverse()
}
