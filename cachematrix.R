# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# get is a function that returns the vector x stored in the main function.
# set is a function that changes the vector stored in the main function.
# set and get are similar to functions to setmean and getmean.
# The "set and get" simply store the value of the input in a variable m.
# into the main function makeVector (setmean) and return it (getmean).

B = matrix( c(2, 4, 3, 1, 5, 7), 
   nrow=3, 
  ncol=2) 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}





# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.


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

# Examples:
# a <- diag(10,4)
# a
# CachedMarix <- makeCacheMatrix(a)
# cacheSolve(CachedMarix)

