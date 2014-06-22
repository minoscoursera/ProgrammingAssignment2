## A pair of functions for calculating and caching the 
## inverse of a matrix. 
## 
## Usage:
##  > m <- matrix(1:4, 2, 2)
##  > cm <- makeCacheMatrix(m)
##  > cacheSolve(cm)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
## Subsequent calls will use the cached solution.
##
## Changing the stored matrix:
##  > cm$set(matrix(c(3, 1, 4, 5, 0, 2, 1, 8, 2, 0, 10, 2, -1, -2, -5, 0), 4, 4))
##  (flushes the cache)
##  > cacheSolve(cm)
##  (computes new inverse)
##
## Peek at the cache:
##  > cm$getCached()


## makeCacheMatrix(x)
##
##  x: matrix
##
## Wraps the given matrix `x' in a list, or cache-matrix.
## The list exposes four functions:
##    l$set(m): sets the wrapped matrix to m
##    l$get(): returns the wrapped matrix
##    l$setCached(v): sets the cached value to v
##    l$getCached(): returns the value in the cache
makeCacheMatrix <- function(x = matrix()) {
  # Cache the inverse here
  cached <- NULL
  
  # Return a list with 'methods' for getting/setting the
  # stored matrix and for getting and setting the cached
  list(
    # Replace the wrapped matrix with a new matrix `m'.
    # Flushes the cache.
    set = function(m) {x <<- m; 
                       cached <<- NULL},
    
    # Returns the stored matrix
    get = function() x,
    
    # Sets the value in the cache to `inv' and returns the value.
    setCached = function(inv) {cached <<- inv; inv},
    
    # Returns the current value in the cache.
    getCached = function() cached)
}

## cacheSolve(x)
##
##  x: cacheMatrix
##
## Call this function with the result of a call to
## makeCacheMatrix().
##
## Calculates the inverse of the cache-matrix, using 
## the cached value, if available. If no value is
## cached, caches the newly-calculated inverse.
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getCached()
  if (is.matrix(cachedInverse)) {
    cachedInverse
  } else {
    x$setCached(solve(x$get()))
  }
}
