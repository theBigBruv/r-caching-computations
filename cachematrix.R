## This function creates a special cache matrix that contains 4 functions
## One to set the value of the matrix
## One to get the value of the matrix
## One to set the value of the matrix inverse
## And the last one to get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'
## However it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise it computes the inverse using the solve function, and sets the inverse value in the cache using the setInverse function

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
