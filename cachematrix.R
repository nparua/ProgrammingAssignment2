## R function that is able to cache potentially time-consuming computation involving
## matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  # inverts the matrix
  setinv <- function(solve) inv.x <<- solve
  getinv <- function() inv.x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  inv.x <- x$getinv()
  if(!is.null(inv.x)) {
    message("getting cached data")
    return(inv.x)
  }
  data <- x$get()
  inv.x <- solve(data, ...)
  x$setinv(inv.x)
  inv.x
}
