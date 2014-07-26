
## "makeCacheMatrix" is doing the similar thing to what "makeVector" does 
## in the example, except that the input is a matrix instead of a vector.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## "cacheSolve" is doing the similar thing to what "cachemean" does 
## in the example, except that the input is a matrix instead of a vector.
##

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}