## This file contains two functions which together illustrates on the
## caching feature in R

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix
# function. If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve retrieves the inverse from the cache and returns it
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}