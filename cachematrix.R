## These functions cache the inverse of a given matrix to avoid repeated computation

## This function sets a list of functions as input for the second fuction cacheSolve()

makeCacheMatrix <- function(x=matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function returns the inverse of the matrix to makeCacheMatrix()

cacheSolve <- function (f, ...) {
    i <- f$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- f$get()
    i <- solve(data, ...)
    f$setinverse(i)
    i
}
