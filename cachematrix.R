## makeCacheMatrix creates a matrix that is cached, and that be inverted by
## cacheSolve. The inverted matrix will of course also be cached, so that it 
## can be retrieved a second time.

## makeCacheMatrix is a function that caches a matrix that can be retrieved by 
## cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) 
}


## cacheSolve is a function that gets the inverse of x from cache if this 
## was called previously with the same matrix x. Else it just inverses x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}