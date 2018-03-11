## JHU "Programming R" Course
## Week #3  Programming Assignment #2
## RD Daudt  3/11/2018
##
## The following functions perform the action of caching the
## inverse of a matrix rather than computing it repeatedly.


## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by 
## "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ## check if he inverse has already been computed
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## compute the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
