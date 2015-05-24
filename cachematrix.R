## These two functions create and cache a matrix in the parent environment and 
## retrieve it without recalculating if the same variable name is used.

## makecacheMatrix creates a matrix variable and four functions for accessing said matrix

makeCacheMatrix <- function(x = matrix()) {
  #instantiate mat
  mat <- NULL
  #establish set function and store variable x looking first in parent environment. Set mat to null
  #to prevent retrieval of old data.
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  #define get for value x. Retrieve value of x
  get <- function() x
  #define setmat to store matrix data in parent environment
  setmat <- function(matrix) mat <<- matrix
  #define getmat function to retrieve variable mat
  getmat <- function() mat
  #list all internal functions so they can be accessed.
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)

}


## cacheSolve returns a previously stored matrix if it exists, otherwise creates a new one

cacheSolve <- function(x, ...) {
        ## Return a matrix that has been previously stored and exit if it exists
  mat <- x$getmat()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  ## Return the inverse of a matrix
  ## First retrieve x
  data <- x$get()
  ## Invert matrix
  mat <- solve(data, ...)
  ## Store inverted matrix in parent environment
  x$setmat(mat)
  ## Return inverted matrix
  mat
}
