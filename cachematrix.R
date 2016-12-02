## Put comments here that give an overall description of what your
## functions do
## Create test matrix
a <- c(1,2)
b <- c(3,4)
test <- rbind(a,b)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##placholder for a future value
  i <- NULL
  ##sets the matrix x to a new matrix y and resets the inverse to null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##returns the matrix
  get <- function() x
  ##sets the inverse to i
  setinverse <- function(solve) i <<- solve
  ##returns the inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
