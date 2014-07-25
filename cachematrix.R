## the function creates a Matrix object that can cache its inverse
makeCacheMatrix <- function( q = matrix() ) {
    i <- NULL
set <- function( matrix ) {##function to create the set matrix
    q <<- matrix
    i <<- NULL
}
get <- function() { ##function to create the get
    q
}
sInverse <- function(inverse) { ##function to set inverse
    i <<- inverse
}
gInverse <- function() { ##function to get inverse
    i
}
##printing/listing the results
list(set = set, get = get,
     sInverse = sInverse,
     gInverse = gInverse)
}
## this function computes the inverse function(makeCacheMatrix)
cacheSolve <- function(x, ...) {
    q <- x$gInverse()
    if( !is.null(q) ) {
        message("getting cached data")
        return(q)
    }
    dt <- x$get()
    q <- solve(dt)
    x$sInverse(q)
    q
}