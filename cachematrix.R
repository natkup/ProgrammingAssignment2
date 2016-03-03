## This function takes matrix as an argument which can then be 
## applied a set of functions. "get()" retrieves the matrix
## "set()" sets a new matrix. "getinv()" retrieves the cached 
## inverse matrix. "setinv()" sets the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function tries to retrieve the cached inverse matrix. 
## If it doesn't exist yet, it calculates the inverse matrix and
## caches it by using the function "setinv()" defined above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
