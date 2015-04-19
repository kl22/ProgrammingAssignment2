## Efficiently calculate and cache inverse of a matrix. 

## Construct matrix object which can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(n) {
        m <<- n
        i <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of a 'cacheable matrix' i.e. an object constructed 
## by the function makeCacheMatrix(). Returns cached inverse if pre-calculated 
## otherwise computes inverse, caches it, and returns value.

cacheSolve <- function(mc, ...) {
        ## Return a matrix that is the inverse of 'mc'
    i <- mc$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- mc$get()
    i <- solve(data, ...)
    mc$setInverse(i)
    i
}
