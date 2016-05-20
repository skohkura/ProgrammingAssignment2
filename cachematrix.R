## makeCacheMatrix creates a representation of a matrix with the ability to cache the inverse. It is a list of functions
## to set a matrix, get the stored matrix, set a cached inverse matrix, and get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks if the inverse of the matrix is cached. If it is, it returns the cached inverse and if it isn't,
## it calculates the inverse and stores it in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
