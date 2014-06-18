## This pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object
##    that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
##
## this special "matrix" object is, really, 
##    a list containing functions to
##
##  . set the value of the matrix
##  . get the value of the matrix
##  . set the value of its inverse
##  . get the value of its inverse
##
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
##
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  ## If the inverse has already been calculated 
  ##  (and the matrix has not changed), 
  ##  then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## otherwise invert the matrix
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  ## and return a matrix that is the inverse of 'x'
  s
}
