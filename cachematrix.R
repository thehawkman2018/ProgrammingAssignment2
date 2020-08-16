## function to make an Invertible CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function to cache an Invertible Matrix

cache <- function(x, ...) {
  cache <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){
      message("retrieve matrix")
      return(invMatrix)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
  }
}
