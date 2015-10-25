## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## These functions will create an object that holds a matrix and caches it's inverse.

## makeCacheMatrix creates a matrix object that is capable of caching it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function calculates the inverse of the matrix created by makeCacheMatrix.
## Step 1: Check if the Inverse has already been calculated.
## Step 2: Return inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}


