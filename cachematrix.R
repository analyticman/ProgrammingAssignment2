## Put comments here that give an overall description of what your
## functions do

## This function contains 4 functions to 
##      (1) set the matrix data to the cache
##      (2) get the cached matrix data
##      (3) set the inverse of the matrix to cache using the solve function
##      (3) get the inverse of the matrix from cache or set it to cache if not previously stored
##

makeCacheMatrix <- function(x = matrix()) {
     ## m is defined in this function but will get used globally using lexical scoping
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) m <<- inverse
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## Function to return the inverse of a square matrix
## The inverse will be returned from cache if previously calculated
## Otherwise, the inverse will be stored in cache for potential future use
##

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## retrieve the values in the cached matrix which is stored in the variable m defined in function makeCacheMatrix
     z <- m$get()
     if (isTRUE(all.equal(z, x))) {     
          y <- m$getInverse()
          if (!is.null(y)) {
               message("getting previously stored cached data")
               return(y)               
          } else {
               z <- m$setInverse(solve(x))
               message("getting cached data for the first time")
               y <- m$getInverse()
               return(y)
          }
     } else {
          m$set(x)
          m$setInverse(solve(x))
          y <- m$getInverse()
          message("new matrix data being stored and inverse calculated")
          y
     }
     
}


