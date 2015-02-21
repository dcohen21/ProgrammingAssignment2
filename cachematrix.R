## These functions are designed to cut down on the amount of
## resources required to find the inverse of a matrix.
## Rather than re-computing the inverse every time it's 
## requested, these cache and retrieve it. The functions assume
## that they will be passed a square, invertible matrix.


## makeCacheMatrix creates a special "matrix-esque" object
## that can cache its inverse. It returns a list of its
## getters and setters.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
              x <<- y
              i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
              i <<- inverse
        }
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse=getinverse)
}


## cacheSolve computes the inverse of the "matrix-esque" object
## returned by makeCacheMatrix. If the inverse has been calculated
## and the matrix has not changed, it will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
              message("retrieving cached inverse")
              return(i)
        }
        squaremat <- x$get()
        i <- solve(squaremat, ...)
        x$setinverse(i)
        i
}
