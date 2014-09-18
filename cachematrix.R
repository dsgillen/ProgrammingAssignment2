## These functions allow for the creation of a "special"
## matrix that can easily be inverted.  The value of the
## inverse of the matrix is cached, so subsequent calls 
## to compute the inverse of the same matrix will return
## a cached value, rather than recomputing the inverse

## An R function to create a matrix whose inverse
## can be cached when the cacheSolve() function is called
## The parameter passed in is a matrix that can be inverted
## Once created, the following functions are available for
## this matrix:
## $get() - retrieve the value of the matrix
## $set(x) - sets the current matrix of this object to x
## $setinverse(x) - stores the inverse of the matrix
## $getinverse() - retrieves a previously stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This is a function to solve the inverse of a matrix
## using a cached value when available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- as.matrix(x$get())
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}

