## This tests the cachable matrix functionality. It knows nothing about how to solve/inverse matrixes.

test.getReturnsMatrix <- function()
{
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix(actual)
    
    checkIdentical(actual, subject$get())
}

test.defaultMatrix <- function()
{
    actual <- matrix()
    subject <- makeCacheMatrix()
    
    checkIdentical(actual, subject$get())
}

test.defaultInverseMatrixIsNull <- function()
{
    subject <- makeCacheMatrix()
    
    checkIdentical(NULL, subject$getinverse())
}

test.getSetOnInverseMatrix <- function()
{
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix()
    subject$setinverse(actual)
    
    checkIdentical(actual, subject$getinverse())
}

test.setMatrixResetsInverseMatrix <- function()
{
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix()
    subject$setinverse(actual)
    subject$set(actual)
    
    checkIdentical(NULL, subject$getinverse())
}

