## These functions store the inverse of a square matrix into a cache.

## This function sets the value of the matrix, gets the value of the matrix, sets the value of the matrix invers, and gets the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the matrices based on the makeCacheMatrix() function.
## If the inverse has already been calculated, it takes the inverse from the cache function above.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
