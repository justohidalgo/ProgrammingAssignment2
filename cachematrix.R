## R functions to enable inverse matrix processing from previous operations
## to be cached so it can be accessed again. 

## This function creates a set of "setters" and "getters" with internal variables
## that store the value of the inverse matrix. This can be used by other
## functions to directly obtain the value without needing to process
## it again.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) m <<- inversematrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve obtains checks the internal inversematrix status of the matrix, so
# that if the inversematrix value is "active", this means it is cached
# and therefore can be directly returned without processing it again.
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
