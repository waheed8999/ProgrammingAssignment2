## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix create a list containing a function to
# a- set the value of the matrix
# b- get the value of the matrix
# c- set the value of inverse of the matrix
# d- get the value of inverse of the matrix
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


# The following function returns the inverse of the matrix. at first it checks if
# the inverse has already been computed. If yes, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache using
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
