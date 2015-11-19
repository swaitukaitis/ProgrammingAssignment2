## This file contains two functions designed to 
## cache the solution to a matrix inversion
## 

## The function makeCacheMatrix creates a matrix-like 
## object that knows to store the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The function cacheSolve will either (a) solve
## for the inverse of the input matrix x if it is not 
## yet calculated, or (b) retrieve the precalculated
## inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
}
