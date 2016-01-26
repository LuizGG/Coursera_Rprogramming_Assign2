## This script contains two functions that for a given matrix, returns its inverse.
## The main purpose of these functions is to cache the inverse matrix in the memory.
## Therefore, it avoids the computational cost of having to rerun the inverse of the matrix mutiple times.

## The makeCacheMatrix function requires a matrix and returns a list to be used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  st <- NULL
  set <- function(y) {
    x <<- y
    st <<- NULL
  }
  get <- function() x
  setinv <- function(inv) st <<- inv
  getinv <- function() st
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve reads the list produced by the makeCacheMatrix function and proceed to do the following steps:
### 1 - Checks if there is already a stored inverse matrix, if true, just returns the saved value.
### 2 - If (1) is false, the functions reads the matrix.
### 3 - Then, through the solve() function it calculates the inverse matrix.
### 4 - Returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  st <- x$getinv()
  if(!is.null(st)) {
    message("getting cached data")
    return(st)
  }
  data <- x$get()
  st <- solve(data, ...)
  x$setinv(st)
  st
}
