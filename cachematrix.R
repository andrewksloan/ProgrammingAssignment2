## This function craetes an object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL

    ## set matrix method
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get matrix method
    get <- function() {
    	## Return the matrix
    	m
    }

    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


### Function to compute the inverse of the matrix — if it has already been
### calculated, then get th inverse from the cache. 
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    # Check to see if the inverse is set in the cache
    if( !is.null(m) ) {
            ## Return cached data 
            return(m)
    }

    ## Otherwise, calculate the inverse using solve(x) function, return 
    res <- x$get()
    m <- solve(res) %*% res
    x$setInverse(m)
    m
}
