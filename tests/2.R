## Tests the \code{cacheSolveWith} method to ensure it correctly caches 
## results and makes use of the cached result on later runs.

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

test.dotDotDotPassedToSolve <- function()
{
    passedParam = F
    actual <- matrix(1:4, 2, 2)
    subject <- makeCacheMatrix()
    stubbedSolve <- function(x, testParam = F, ...) {
        print(testParam)
        passedParam <<- testParam
    }
    cacheSolveWith(stubbedSolve, subject, testParam = TRUE) 
    
    checkIdentical(TRUE, passedParam)
}

