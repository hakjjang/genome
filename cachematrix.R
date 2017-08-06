## Put comments here that give an overall description of what your
## functions do
## These functions allow the user to store the inverse of a matrix in the cache.

## Write a short comment describing this function
## The functions below are used to cache the inverse of a matrix, which allow the user to retrieve it.

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invers <<- inverse
  getinverse <- function() invers
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of a matrix from a makeCacheMatrix() object.

cacheSolve <- function(x, ...) {
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  dat <- x$get()
  invers <- solve(dat)
  x$setinverse(invers)
  invers
}
