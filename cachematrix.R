## The functions makeCacheMatrix and cacheSolve will cache and return the 
## inverse of a matrix. 
## The inverse will be retrieved from the cache if possible, thereby lowering 
## computational costs.
## The functions assume that the matrix is invertible.


## Given a matrix, x, makeCacheMatrix returns a list of 4 functions:
##
## get()        :  gets the value of x 
## set()        :  sets the value of x 
## getInverse() :  gets the inverse of x 
## setInverse() :  sets the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve returns the inverse of a matrix that has been processed by the
## function makeCacheMatrix.
## If available, cacheSolve will retrieve a previously computed matrix inverse 
## from the cache. Otherwise, cacheSove will compute the inverse and store it
## in the cache for future use.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
