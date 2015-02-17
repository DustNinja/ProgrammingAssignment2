## These functions are designed to calculate the inverse of a 
## matrix using caching to potentially reduce computational cost. 

## The makeCacheMatrix function creates a special matrix object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) cachedInv <<- inverse
    getInv <- function() cachedInv
    list(set = set, 
         get = get, 
         setInv = setInv,
         getInv = getInv)
}


## The cacheSolve function computes the inverse of the special
## "matrix" returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInv(inverse)
    inverse
}
