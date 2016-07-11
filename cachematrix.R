## Since matrix inversion is a costly computation, there might be some benefit
## of caching the inverse of a matrrix. The following functions are used to cache
## the inverse of a matrix by storing already computed values and avoids recomputation
## and thereby potential computations can be avoided

## This function is used to create a list that obtains the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function actually caches the inverse of a matrix. First it checks if inverse of 
## a matrix is already computed and if so, it skips the computation step. If not, inverse is computed 
## and the new value in stored in cache through the set inverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
