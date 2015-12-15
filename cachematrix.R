## Put comments here that give an overall description of what your

## This function is used to create a special matrix, for which the inverse operations not only
## calculates the inverse, but also caches it to save on future calculations.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL  #setting inverse initially to 0.
     
     set  <- function(y){ #setting the value of the matrix to the argument.
          x <<- y         
          inverse  <<- NULL  #inverse is set to Null for the new data.
     }
     
     get  <- function() x  #return the matrix
     
     setInverse  <- function(inversecomputed){ #function to set Inverse if it has not been caculated before           
          inverse <<- inversecomputed   #the computed inverse is cached.
     }
     
     getInverse  <- function() inverse  #retrieve inverse from cache
     
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #the list of 4 functions of makeCacheMatrix. 
}


## This function is used to obtain the inverse of a matrix. THe function first checks the cache
## to see if the inverse already exists, otherwise the inverse is computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse  <- x$getInverse()  #retrieve the inverse
     if(!is.null(inverse)){  #if inverse is not null, i.e. cache has inverse ...
          message("Getting cache data")
          return(inverse)  #...use the cache and end the function
     } #if here, then cache doesn't exist.
     data  <- x$get()  #get the data and ...
     inverse  <- solve(data,...) #... then compute the inverse and ...
     x$setInverse(inverse)  #...finally store the computed inverse in cache.
     inverse                #return the inverse to the user.
}
