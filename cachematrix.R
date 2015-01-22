# Returns an 'inverse cacheable' matrix. The matrix value and the inverse are 
# stored in closures
#
# Args:
#   x: the matrix that we want to cache
#
# Returns:
#   A list of four functions:
#   set(data) - sets the value of the matrix
#   get() - returns the value of the matrix (set as x from above)
#   setinverse(data) - sets the cached inverse value of the matrix
#   getinverse() - gets the cached inverse value of the matrix, 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Solves the inverse of an 'inverse cacheable' matrix. If the value has 
# already been computed, we use the cached value
#
# Args:
#   x: the inverse cacheable matrix, of the type created by makeCacheMatrix()
#
# Returns:
#   A matrix that is the inverse of the underlying matrix of the provided
#   cacheable matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  matr <- x$get()
  
  inv <- solve(matr, ...)
  x$setinverse(inv)
  
  inv
}
