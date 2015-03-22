## Caches the matrix inverse, as it is an expensive operation
## 
## Steps followed:
## Given a matrix, checks in the cache if the inverse already exist.
##      a) If present, returns the result from cache.
##      b) Else, calculates the inverse, adds it to the cache and 
##          returns the inverse

## makeCacheMatrix(x)
## Creates a cache of a given matrix along with 
## its inverse value. 
## * Client should call "setInverse" to cache the result. 
## * First time (or before the call to setInverse) 
##    the inverse value in cache would be NULL.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve(x)
## Computes the inverse of a matrix.
## Leverages the cache if it is already computed.
## Input matrix is created using makeCacheMatrix()
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse of the matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
