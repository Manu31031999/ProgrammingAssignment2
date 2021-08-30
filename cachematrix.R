## Put comments here that give an overall description of what your
## functions do
## They cache the inverse of a matrix

## Write a short comment describing this function
## It generates a matrix object able to cache it inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Starting the inverse propierty
  i <- NULL
  ## Setting the matrix 
  set <- function (matrix) {
    m <<- matrix
    i <<- NULL
  }
  ## Getting and returning the matrix
  get <- function() {
    m
  }
  ## Now we are setting the inverse of our matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  ## Finally we are getting the inverse 
  getinverse <- function() {
    i
  }
  ## And a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Calculates the inverse of the matrix generated before. If it's been calculated
## the function retrieves the value from the memory
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## Tries to recover the inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculates the inverse 
    m <- solve(data) %*% data
    
    ## Sets the inverse to the object
    x$setinverse(m)
    
    ## Returns the matrix
    m
  }
  
  