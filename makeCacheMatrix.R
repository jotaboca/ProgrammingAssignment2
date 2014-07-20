makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     setMatrix <- function(y) {
          x <<- y
          inv <<- NULL
     }
     getMatrix <- function() {
          x
     }
     cacheInverse <- function(inverse) {
          inv <<- inverse
     }
     getInverse <- function() {
          inv
     }
     
     list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, 
          getInverse = getInverse)
     }