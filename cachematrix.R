## makeCacheMatrix - creates the inverse of a given inversible matrix and 
## cache the results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve - returns the inverse of a given inversible matrix first checking 
## if the result was calculated before and it is cached. 

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
