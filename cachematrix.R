## The first function gets caches the inverse of a matrix. 
##The second function checks to see if the matrix has been cached and solves for the inverse if it's not in the cache already.
##Caching can save a lot of time when working with large amounts of data.

## Create test matrix
a <- c(1,2)
b <- c(3,4)
test <- rbind(a,b)

## This is the function that caches the inverse of a matrix.

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


## This function computes the inverse of the "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated by makeCacheMatrix and the matrix has not changed,
##cacheSolve will use the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
