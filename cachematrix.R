## This program allows to compute the inverse of a matrix and cache the result
## to retrieve it from the cache whenever call to this function.

## makeCacheMatrix() 
## Input data : matrix "x" to be created
## Output data : list of functions -get, set, getInverse, setInverse-
## set : sets the data of the matrix "x"
## get : gets the data of the matrix "x"
## setinv : sets the inverse of the matrix "x"
## getinv : gets the inverse of the matrix "x"

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


## cacheSolve()
## Input data : the matrix "x" whose inverse needs to be computed
## Output data : the inverse of the matrix "x"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of "x"
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
