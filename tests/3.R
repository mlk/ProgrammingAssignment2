test.allTogetherNow <- function() {
    actual <- matrix(c(-2, 1, 1.5, -0.5), 2, 2)
    subject <- makeCacheMatrix(matrix(1:4, 2, 2))
    result <- cacheSolve(subject)
    
    checkIdentical(actual, result)
    checkIdentical(actual, subject$getinverse())
}