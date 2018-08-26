# These functions allow to compute the inverse of a matrix and also to cache the result.
# If the inverse of the same matrix is required, it can be looked up in the cache rather than recomputed


# The first function, makeCacheMatrix, creates a special "matrix"
# which is really a list containing a function to
#   1. set the value of the vector
#   2. get the value of the vector
#   3. set the value of the mean
#   4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  cachedinv <- NULL
  set <- function(y) {
    x <<- y
    cachedinv <<- NULL # invalidate cache since data changed
  }
  get <- function() x
  setinv <- function(inv) cachedinv <<- inv
  getinv <- function() cachedinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


# Tests
#m1 <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
#cacheSolve(m1) # first call computes inverse
#cacheSolve(m1) # second call gets inverse from cache
#cacheSolve(m1) %*% m1$get() # matrix multiplication gives identity : OK
