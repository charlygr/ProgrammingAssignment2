## This functions compute the inverse of a matrix and cache the result. If matrix
## does not change, a call to cacheSolve doesn't compute the inverse again.
## It must call makeCacheMatrix before cacheSolve

## This function,creates a special "matrix", which is really a list containing a
## function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() solve(x)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mdat <- x$get()
  inv <- solve(mdat, ...)
  x$setInverse(inv)
  inv
}
