test.setsInverseMatrixWithResultOfPassedSolve <- function()
{
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix()
    stubbedSolve <- function(...) actual
    
    cacheSolveWith(stubbedSolve, subject)
    
    checkIdentical(actual, subject$getinverse())
}

test.doesNotRecaculateIfAlreadySolved <- function()
{
    attemptsReCaculate = F
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix()
    stubbedSolve <- function(...) attemptsReCaculate <<- T
    subject$setinverse(actual)
    
    cacheSolveWith(stubbedSolve, subject)
    
    checkIdentical(FALSE, attemptsReCaculate)
}
