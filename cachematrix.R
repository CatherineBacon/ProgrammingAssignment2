## These functions cache the inverse of a matrix

## This function creates a special "matrix" objet that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initially set i to NULL
  i <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## This function calculates the inverse of the special "matrix" created above
## x should be an instance of makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## get inverse matrix from makeCacheMatrix
  i <- x$getinverse()
  
  ## Check to see if the inverse has already been calculated
  if(!is.null(i)) {
    ## Get cached inverse matrix and return it
    message("getting cached data")
    return(i)
  }
  
  matrix <- x$get()
  
  ## solve for inverse matrix
  i <- solve(matrix, ...)
  
  ## now inverse is calculated, cache it
  x$setinverse(i)
  
  ## Return inverse matrix
  i
}