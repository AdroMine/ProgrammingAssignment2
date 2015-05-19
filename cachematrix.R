## Put comments here that give an overall description of what your

## This function is used to create a special matrix, for which the inverse operations not only
## calculates the inverse, but also caches it to save on future calculations.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set  <- function(y){
          x <<- y
          inverse  <- NULL
     }
     get  <- function() x
     setInverse  <- function(inverse) inverse <<- inverse
     getInverse  <- function() inverse
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is used to obtain the inverse of a matrix. THe function first checks the cache
## to see if the inverse already exists, otherwise the inverse is computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse  <- x$getInverse()
     if(!is.null(inverse)){
          message("Getting cache data")
          inverse
     }
     data  <- x$get()
     inverse  <- solve(data,...)
     x$setInverse(inverse)
     inverse
}
