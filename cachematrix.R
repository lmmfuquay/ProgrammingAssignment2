## Week 3 Assignment: Write a pair of functions that cache the inverse of a matrix 
## in order to save potential computing time.
## One function creates a matrix object that can cache its inverse. The second 
## function computes the inverse (or retrieves if already calculated and cached).


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL       
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.
## Notes: Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
