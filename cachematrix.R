## Matrix inversion is a costly computation process and hence it helps to cache the inverse of a matrix rather
## than computing the inverse repeatedly. The following functions help cache the inverse of a matrix

## MakeCacheMatrix function is used to set and get the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() iv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of a matrix. If the inverse is already computed, it gets the result and thereby
## skips the computation. If not, it computes the inverse and stores the value in cache via setinverse function

cacheSolve <- function(x, ...) {
    iv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
