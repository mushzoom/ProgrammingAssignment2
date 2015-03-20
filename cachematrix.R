## There are two functions in this cachematrix.R file: 
##
## 1) makeCacheMatrix 
## 2) cacheSolve
##
## These functions are written to help computations of the inverse of matrices. 
## The rationale for the above is that, it is generally accepted that computing the 
## inverse of a matrix is a resource-intensive operation. 

## Hence we cache the computations as we go along, however
## the assumption is that the matrix supplied must be invertible, 
## else these functions will not output the correct answers. 
## 

## makeCacheMatrix takes an invertible matrix as input and
## returns a special "matrix" object, 
## from which a cache of its inverse can be obtained.

makeCacheMatrix <- function(x = matrix()) {
  # 
  # Input: an invertible matrix 
  # e.g. c <- rbind(c(1, -1/4), c(-1/4,1))
  # test by solve(c)%%c
  # it should return the identity matrix.
  # If so, the matrix is invertible. 
  
  # initialize inv to NULL
  inv <- NULL
  
  # function setme stores both x and inverse away from the current environment
  setme <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # function getme retrieves the original matrix passed-in, into the current environment
  getme <- function() {
    return x
  }
  
  # function setinverse stores the inverse away from the current environment
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # function getinverse retrieves the inverse into the current environment
  getinverse <- function() {
    inv
  }
  
  # Output: 
  # A list of functions available for using with the cacheSolve function.
  # This list can be accessible by '$'<name of function> in cacheSolve
  list (setme = setme, getme = getme, setinverse = setinverse, getinverse = getinverse)

}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated AND the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # 
  # Input: the result from makeCacheMatrix (it will be a list) 
  # e.g. a <-- makeCacheMatrix(c)
  # where c is an invertible matrix
  # e.g. c <- rbind(c(1, -1/4), c(-1/4,1))
  # 
  
  # retrieve the inverse into the current environment
  inv <- x$getinverse()

  # check to see if the inverse has been calculated and matrix has not changed
  if(!is.null(inv)) {
    message("Getting the cached inverse")
    return(inv)
  }
  
  # get the original matrix stored into the current environment
  mystuff <- x$getme()
  
  # solve is a built-in R function that computes the inverse of a matrix
  inv <- solve(mystuff, ...)
  
  # store the inverse outside of the current environment
  x$setinverse(inv)
  
  # Output: the inverse of the matrix supplied in makeCacheMatrix
  inv
  
}
