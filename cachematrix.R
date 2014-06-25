## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: create a list of four functions for a given variable.  
## 1: variable$setMatrix() store a matrix
## 2: variable$getMatrix() return stored matrix
## 3: variable$cacheInverse() stores the inverse matrix of variable$getMatrix(), once solved by
##                            cacheSolve()
##                            (perhaps better would be to incorporate cacheSolve() into $cacheInverse)
## 4: variable$getInverse() returns the cached inverse matrix if it has already been solved.

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

## cacheSolve: return a matrix that is the inverse of 'x' and store in cache. Before attempting 
## to solve, check to see if solution is already stored in cache.  if so, return cached matrix
## instead of re-solving.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$getMatrix()
     inv <- solve(data)
     x$cacheInverse(inv)
     inv
}


## check using 
## > x = matrix(c(4,3,3,2), nrow = 2, ncol = 2)
## > test <- makeCacheMatrix(x)
## original matrix:
## > test$getMatrix()
##      [,1] [,2]
## [1,]   4    3 
## [2,]   3    2
## results of inverse solve:
## > cacheSolve(test)
##      [,1] [,2]
## [1,]  -2    3
## [2,]   3   -4