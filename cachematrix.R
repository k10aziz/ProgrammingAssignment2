##This file contains 2 functions, one for set and get functions for data 
#abstraction, and the other for calculating and caching the result.

## This function creates get/set functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   x_i <- NULL # variable to store inverse of matrix
   
   # Set function for matrix created by makeCacheMatrix function
   set_Matrix <- function(y) {
      x <<- y
      x_i <<- NULL 
   }
   
   #get function for the input matrix
   get_Matrix <- function() x 
   
   #set inverse of matrix
   set_MatrixInv <- function(inv) x_i <<- inv
   
   #get inverse of matrix
   get_MatrixInv <- function() x_i
   
   list(set = set_Matrix, get = get_Matrix, setInv = set_MatrixInv, getInv = get_MatrixInv)
}

#This function checks if the inverse exists. Returns the cached inverse if it 
#exists, otherwise calculates and returns the inverse.
cacheSolve <- function(x, ...) {
   ## Return inverse matrix of x
   m_i <- x$getInv()
   
   if(!is.null(m_i)) { 
      return(m_i)
   }
   data <- x$get() 
   m_i <- solve(data)
   x$setInv(m_i) 
   m_i 
}
