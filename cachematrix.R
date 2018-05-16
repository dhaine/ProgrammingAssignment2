## Function that computes the inverse of a matrix, caching its result.

## 1. makeCacheMatrix creates a list of functions that have the same 
## parent environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix from makeCacheMatrix.
## If the matrix has not changed and the inverse already calculated,
## return the cache inversed matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Example
## matrix_1 <- matrix(c(2, 4, 6, 8), 2, 2)
## matrix_1c <- makeCacheMatrix(matrix_1)
## cacheSolve(matrix_1c)
## cacheSolve(matrix_1c)
