# A set of functions that return the inverse of a matrix.
# The inverted matrix is cached for repeated use.

# Takes a matrix and returns a list of attributes that can be called.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(m) {
            x <<- m
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set=set, get=get, 
           setinverse=setinverse, 
           getinverse=getinverse)
      
}


# Takes a matrix of the type makeCacheMatrix. Caches and returns the inverse
# of the input matrix.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}