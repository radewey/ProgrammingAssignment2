## R-Programming Week 3, Assignment 2

## makeCacheMatrix() takes a matrix and stores it in a list of functions which set and retrieve
## the matrix and its inverse. cacheSolve() accepts lists of functions, spcifically results from 
## makeCacheMatrix(). cacheSolve() takes the list and determines if the inverse has been calculated.
## if it has, it retrieves the stored inverse and does not have to recalculate it.

## makeCacheMatrix() sets values and creates functions to retrieve those values.
## The function recives a matrix, stores it, inverts it, and stores the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## holds the inverse with a default NULL
  set <- function(y) {   ## set() will set the matrix
    x <<- y ## use <<- because x and i are from mackeCacheMatrix, and do not live in set()
    i <<- NULL 
  }
  get <- function() x   ## get will retrieve the matrix
  setsolve <- function(solve) i <<- solve    ## solve the matrix and set it as the value of i
  getsolve <- function() i    ## getsolve() will retrieve i
  ## returns a list of the values of the functions above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve() accepts a list as an argument. It then chcks the list to see if there are
## values stored in it. If there is a value stored in the fourth position (getSolve()), then 
## it retrives that stored value, but if it is not stored, it must calculate it and store the 
## results. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()   ## check the list 'x' for a stored inverse matrix
  if(!is.null(m)) {   ## if it is stored, return "getting cached data" and the cached inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get()  
  m <- solve(data, ...)
  x$setsolve(m) ## 
  m
}