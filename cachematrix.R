## 2// Programming Assignment 2 - R Programming - June 2014

## This assignment is to write 2 functions to cache the function of a matrix. 
## We assume that the matrices inout will always be invertible.
## The 2 functions are as follows:
## 1. makeCacheMatrix(): This function creates a special matrix object
## that can cache its inverse.
## 2.  cacheSolve(): This function computes the inverse of the special
## matrix returned by makeCacheMatrix() above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

## MakeMatrix() function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize a null object to hold matrix inverse
  i <- NULL 
  
  # This set function is used to set the value of the special matrix
  set <- function(y) {
    # x gets assigned the input matrix y. This value x is now available globally.
    x <<- y
    i <<- NULL
  }
  
  # This get function retrieves the value of the special matrix
  get <- function() { x }
  
  # This setinv function sets the inverse calculated to the object i. Note that this i 
  # value is now available globally.
  setinv <- function(inv) { i <<- inv }
  
  # This getinv function returns the inverse calculated
  getinv <- function() { i }
  
  # This list is the special object that holds the values returned from the above fucntions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function actually computes the matrix inverse using the prebuilt solve() if the matrix 
## inverse isn't already calculated. If it is is already available, then the value is returned
## as the value i

cacheSolve <- function(x, ...) {
  
  # Get the value of the inverse into i from the getinv() function for the matrix x
  i <- x$getinv()
  
  # If the value of i is NULL, that means it hasn't been precalculated. 
  if(!is.null(i)) {     
    print ("Retrieving cached inverse for the input matrix")
    # Returb the cached inverse
    return(i)
  }
  
  # If the value of i were NULL above, R control will come to this point and calculate
  # the inverse using solve(). matr holds the matrix whose inverse we want to compute
  matr <- x$get()
  i <- solve(matr, ...)
  
  # This calculated inverse i is then loaded to the special matrix object using
  # the setinv() for future use
  x$setinv(i);
  
  # Return the calculated inverse
  i                           
}