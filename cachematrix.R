## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(matrix) {
              x <<- matrix
              m <<- NULL
       }
       
       get <- function() x
       
       setInverse <- function(solve) m <<- solve
       getInverse <- function() m
       
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
       
}


## This function calculates the inverse of makeCacheMatrix, 
#unless it has not changed. If it has not changed, it will 
#receive the inverse from the cache.

cacheSolve <- function(x, ...) {
       m <- x$getInverse()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data) %% data
       x$setInverse(m)
       m
       ## Return a matrix that is the inverse of 'x'
}
