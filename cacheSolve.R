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