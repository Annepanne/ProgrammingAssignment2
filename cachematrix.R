## Inverses a matrix and store the result in the
## cache so that it'll work faster

## Inverses the matrix ad puts it in the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- solve(x)
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getInverse()
    if (!is.null(m) && !is.na(m)) {
        message("getting cahed data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m

}
