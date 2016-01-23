# These functions provide a efficient way to calculate the inverse of 
# a matrix multiple times, caching the result after the first run.
# Take care that you don't get memory problems using these functions!

# 'makeCacheMatrix' creates an object for matrixes where you can:
# 1) set and get the matrix itselfe
# 2) set and get the inverse of the matrix (in case it has been calculated using 'cacheSolve')
makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
        x <<- y
        mmatrixInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) matrixInv <<- inv
    getInv <- function() matrixInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


# 'cacheSolve' returns the inverse of a matrix created using 'makeCacheMatrix'.
# It uses solve to calculate the inverse but caches the result,
# such that further calls just return the inverse from the memory.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}
