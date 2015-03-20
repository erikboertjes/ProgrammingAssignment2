## These functions cache the inverse of a matrix. Since calculating
## the inverse of a matrix, caching it (instead of calculating it over
## and over again) gives performance benefits.

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## variable that stores the inverse of the matrix
  
  ## function for setting the value of the matrix. It clears the cache variable.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## function for getting the value of the matrix. It simply returns the matrix
  ## (which is stored in the variable x)
  get <- function() x
  
  ## function for setting the inverse of the matrix. This function is used
  ## by the cacheSolve function below, in case the inverse of the matrix
  ## was not found in the cache, and needed to be calculated.
  setinverse <- function(inverse) i <<- inverse
  
  ## function for getting the inverse of the matrix
  getinverse <- function() i
  
  ## return the list of the above-defined functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve retrieves the inverse
## from the cache, saving valuable calculation time.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## get the inverse, if any, from the cache
  
  ## if there was an inverse stored in the cache, return it...
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## ...else calculate the inverse and store it in the cache for future use.
  mat <- x$get()   ## get the matrix from x
  i <- solve(mat)  ## calculate the inverse
  x$setinverse(i)     ## store the inverse in the cache of matrix x
  i                   ## return the inverse as the result of this function
}
