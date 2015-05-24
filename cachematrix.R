## These functions help with calculating large matrices' inverse by the ability of caching the calculated inverse.
## To achieve this, one function creates a special matrix, while the other calculates the inverse 
## f it weren't calculated before.

## This function creates a special matrix with variable to preserve the computed inverse.
## Additional methods are available to get and set the inverse and the matrix itself.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns the inverse of the matrix given as the argument.
## The matrix should be a special one, constructed by the function above.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
