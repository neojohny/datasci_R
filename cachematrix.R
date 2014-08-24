## These two funcitons will set the matrix and calculate 
## the inverse of it.

## This makeMatrix will set up the matrix and it's inverse.

makeMatrix <- function(x = numeric()) {
  ## set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## calculate the inverse of the matrix
  setinv <- function(invmatrix) inv <<- invmatrix
  getinv <- function() inv
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
} 

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=numeric(), ...) {
  
  ## get the inverse of the matrix
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if not: get the inverse of the matrix   
  matrix<-x$get()
  inv<-solve(matrix, ...)
  ## set the inverse of the matrix 
  x$setinv(inv)
  inv
}
}
