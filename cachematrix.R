## Caching the inverse of a Matrix

## this function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) iv <<- solve
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function computes the inverse of the sepcial
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already calculated (and the matrix has
## not changed), then the cacheSolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- NULL
  if(! is.null(iv) ){
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)

  iv
}
