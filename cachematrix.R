## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # declare inv as NULL, later it will store inversion of matrix x
  inv <- NULL
  # declare basic method
  
  set <- function(y) {
    # sets x value to be y
    x <<- y
    inv <<- NULL
  }
  get <- function() x # returns x
  
  setinverse <- function(inverse) inv <<- inverse #sets value of inv to be equal to inverse
  getinverse <- function() inv # returns inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # reads value of inv associated with object x
  if(!is.null(inv)) {
    # if inv is not NULL then return cached data
    message("getting cached data")
    return(inv)
  }
  # if inv is NULL then inverse of x hasn't be calculated
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}