## The function here implemented is able to cache the inverse of a
## matrix, rather than compute it repeatdly. This means that if you ask 
## for the inverse of a matrix A more than once, it does not computes the
## inverse each time, it caches the inverse and gives you it back. 


## The makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                   xinv <- NULL
                   setMatrix <- function(y) {
                                x <<- y
                                xinv <<- NULL
                                }
                   getMatrix<- function() x
                   setInverse <- function(Inverse) xinv<<- Inverse
                   getInverse <- function() xinv
                   list(setMatrix = setMatrix, getMatrix = getMatrix,
                        setInverse = setInverse,
                        getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special matrix
## returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
              ## Return a matrix xinv that is the inverse of 'x'
              xinv <- x$getInverse()
              if(!is.null(xinv)) {
                 message("Getting cached data")
                 return(xinv)
              }
              data <- x$getMatrix()
              xinv <- solve(data, ...)
              x$setInverse(xinv)
              xinv
}
