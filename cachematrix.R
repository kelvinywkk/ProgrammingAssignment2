## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialise the inverse property
  invProperty <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    invProperty <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse value of the matrix
  setInverse <- function(i) invProperty <<- i
  
  ## get the inverse value of the matrix
  getInverse <- function() invProperty
  
  ## a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse()
  
  ## check if the inverse has already be calculated
  if( !is.null(invMatrix) ) {
    message("retrieve the inverse from the cache")
    ## retrieve the inverse from the cache
    return(invMatrix)
  }
  
  ## retrieve the matrix 
  matrixData <- x$get()
  
  ## compute the inverse using solve function in R
  invMatrix <- solve(matrixData) 
  
  ## set the inverse value of the matrix
  x$setInverse(invMatrix)
  
  ## return the matrix
  invMatrix        
}
