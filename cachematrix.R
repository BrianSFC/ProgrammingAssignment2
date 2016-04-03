## This set of functions creates a matrix object whose inverse can be cached,
## saving computation time in cases where the inverse has already been calculated.

## makeCacheMatrix creates a special matrix object whose inverse can be cached
## (set and retrieved). In addition, the matrix can be reset with new data using
## the set(y) sub-function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve returns an inverse matrix using the special matrix object
## created by makeCacheMatrix. First it checks to see if the matrix has already
## been solved and stored. If it hasn't, cacheSolve solves the matrix and then
## stores the inverse result. cacheSolve assumes that the argument matrix is
## invertible and returns an error otherwise.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
