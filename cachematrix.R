## makeCacheMatrix creates an object wrapping a matrix.
## This function will cache the inverse of this matix.
## 
## cacheSolve given a makeCacheMatrix will solve or calculate 
## the inverse of a matrix. 

## Given a matrix create an object that can 
## cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## create a cache object with a matrix.
  ## inverse calculated on demand.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)

}


## Given a matrixcache object create or return 
## the cache of that matrix's inverse.
cacheSolve <- function(x = makeCacheMatrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
