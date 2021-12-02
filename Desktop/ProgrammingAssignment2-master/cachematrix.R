## Put comments here that give an overall description of what your
## functions do
## This is a pair of matrix inversion functions created to catch the
## inverse of a matrix. makeCacheMatrix creates the special matrix 
## that can cache the inverse while cacheSolve computes the inverse.

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix that lists the functions 
## to set or get the elements of the matrix (set and get), set or get the elements
## of the matrix inverse (setInverse and getInverse).
##

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
       x <<- y
       inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list (set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve computes for the inverse of the special matrix by
## makeCacheMatrix. 

cacheSolve<- function(x, ...) {
  ## Return a matrix that in the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
    }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv
}
