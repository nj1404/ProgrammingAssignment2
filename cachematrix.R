## makeCacheMatrix is a function that stores a matrix and a cached value of the inverse matrix
## It contains the following functions - setMatrix, getMatrix, cacheInverse and getInverse

makeCacheMatrix <- function(x=numeric()) {
  cache <- NULL
  setMatrix <- function(newValue) {
    x <<-newValue
    cache <<- NULL
  }
  getMatrix <- function(){
    x
  }
  
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  getInverse <- function() {
    cache
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## THe following function calculates the inverse of matrix created by the first function

cacheSolve <- function(y, ...) {
  inverse <- y$getInverse()
  
  if (!is.null(inverse)) {
    message ("Getting cached data")
    return (inverse)
  }
  
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  inverse
}
