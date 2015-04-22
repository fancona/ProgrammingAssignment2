## The first function creates a matrix whose inverse can be cached.
## The second function checks a matrix. If its inverse has not been 
## calculated yet (and the matrix has not been modified), than it calculates its inverse. Otherwise
## it retrieves its inverse from the cached.

## This function is a list of four functions: set, get, setinverse, getinverse
## The set function caches the input (a matrix) by using
## the operator <<- to change the value of x to y, within the main function (makeCacheMatrix)
## instead of changing it within the set function only.
## The get function simply returns the matrix x stored in the main function
## The function setinverse assigns its input to an object (inv) within the main function
## The function getinverse simply returns the object inv
## The last line of the function creates a list of the above four functions
## This is so that each individual function can be subset from the list using the $ operator

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function first checks that the variable inv (originally defined in makeCacheMatrix)
## exists and it is not null. If this is the case, the function cacheSolve
## returns a message and the variable inv.
## Else, it takes the a matrix x and calculates its inverse and stores it
## in the object inv. Then it prints inv.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
