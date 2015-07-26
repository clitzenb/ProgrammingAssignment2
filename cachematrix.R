## The function found below calculates the inverse of a matrix, 
## saves it, and provides the saved inverse calculation if
## the user attempts to calculate the inverse of the same matrix.

## makeCacheMatrix is a function that returns a list of functions
## Its puspose is to store a matrix and a cached value of the  
## inverse of the matrix. Contains the following functions:
## setmatrix  -> set the value of a matrix
## getmatrix  -> get the value of a matrix
## setinverse ->  get the cahced value (inverse of the matrix)
## getinverse ->  get the cahced value (inverse of the matrix)

makecachematrix <- function(x = matrix()) {
  ## define and sets cache to NULL
  cache <- NULL
  ## store matrix 
  setmatrix <- function(value) {
    ## assign x to value in parent environment
    x <<- value
    ## re-initialize cache in parent environment to NULL
    cache <<- NULL
  }
  ##return the matrix x
  getmatrix <- function() x 
  ##set cache equal to the inverse of the matrix x
  setinverse <- function(inverse) cache <<- inverse
  ##give the cached inverse of x
  getinverse <- function() cache 
  #return a list; each named element of the list is a function
  list(setmatrix = setmatrix, getmatrix = getmatrix, cacheinverse = 
         cacheinverse, getinverse = getinverse)
}


## The function found below calculates the inverse of the special
## "matrix" found above. First, the function checks if the inverse
## has been calculated; if it has, the function pulls from the
## cache and skips computation. Otherwise, matrix inverse is 
## calculated

cachesolve <- function(x, ...) {
  ## get inversed matrix of x
  inverse <- x$getinverse()
  ## returns cached value if it exists
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## otherwise calculate matrix and store it
  data <- x$getmatrix()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  ##return inverse
  inverse
}
