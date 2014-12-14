## makeCacheMatrix creates a matrix the caches its inverse for optimization porpouses
## cacheSolve returns the inverse of the matrix received by params. If the matrix has a
##   cached inverse, it is returned, besides, the inverse is calculated and cached

## makeCacheMatrix creates a matrix with helpers methods for caching porpouse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- (function(y)) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the x matrix. Use cache for optimization

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inv <- solve(x, ...)
    x$setinverse(inv)
    inv
}
