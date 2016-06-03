## These functions are used to help cache potentially time consuming matrix
## inversions.

## Provides a list of functions you can use to set and retrieve the matrix
## and it's inversion.

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


## Given a matrix, calculates it's inversion.  The result is cached for fast
## retrieval later.

cacheSolve <- function(x, ...) {

    # Attempt to get result from the cache        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("from cache...")
        return(inv)
    }
    
    # Calculate the matrix inversion
    data <- x$get()
    inv <- solve(data)
    
    # Cache the result
    x$setinverse(inv)
    
    inv
}
