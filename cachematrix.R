## This file contains scripts to create a cacheable matrix inverse

## The data structure for the cacheable matrix inverse,
## with possibilities to assign and return the matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   
   #Set value of matrix to y and reset the inverse to null
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   
   #Return the matrix
   get <- function() x
   
   #Set inverse matrix to inv
   setinverse <- function(inv) inverse <<- inv
   
   #Retun the inverse matrix
   getinverse <- function() inverse
   
   #Declare functions as a list
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Function for calculating inverse of a matrix 
## or returning cached inverse if already calculated
cacheSolve <- function(x, ...) {
   ## Get inverse value from CacheMatrix data structure
   inv <- x$getinverse()
   
   #If inverse exists, just return it
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   #If inverse doesn't exist, calculate it
   data <- x$get()
   inv <- solve(data, ...)
   #And set value of inverse in CacheMatrix for future use
   x$setinverse(inv)
   inv
}
