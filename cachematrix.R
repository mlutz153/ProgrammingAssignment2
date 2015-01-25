## This function is used to compute the inverse of a 
## matrix through the use of the solve function; storing the result
## in memory.

## makeCacheMatrix function calculates the inverse of 
## a matrix.  makeCacheMatrix will search through the 
## current environment first, and then the parent 
## environment for variables assigned to s and x.  It will then solve the 
## inverse of the matrix.



makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve will search the current environment for an assigment to variable
## s, then the parent environment(makeCacheMatrix).  If there is already a value 
## assigned to it, then the function will return it.  
cacheSolve <- function(x, ...) {
  ## This function  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}
