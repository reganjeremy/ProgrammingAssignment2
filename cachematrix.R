## Author: Jeremy Regan
## Date: 4/15/16
## File: cachematrix.R
## 
## cachematrix.R consists of two functions, 
## makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix returns an object with matrix functions (set, get, setinverse, getinverse).
## set: sets the matrix
## get: returns the matrix
## setinverse: calculates and caches the inverse of the matrix using solve
## getinverse: returns the inverse of the matrix
##
## cacheSolve returns the inverse of the matrix using functions
## from makeCacheMatrix. If the invserse has already been calculated
## the cached inverse is returned.
##
## Example:
##
## m <- makeMatrixInverse(matrix(c(1,2,3,4),2))
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## makeCacheMatrix returns an object with matrix functions (set, get, setinverse, getinverse).
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Return a matrix
  get <- function() x
  # Set the inverse of a matrix using solve
  setinverse <- function(solve) m <<- solve
  # Return the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Calculates and returns an inverse of a matrix or 
# returns a cached inverse if the inverse was previously calculated.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # Retrun the cached inverse if m exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Calculate and return the inverse of m
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
