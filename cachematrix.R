## Provides a mechanism to cache the inverse of a matrix - a
## potentialy expensive operation that we only want to perform
## once if possible.


## makeCacheMatrix
## Returns a 'matrix' object that can be used to safely cache the
## inverse of that matrix.
## x -> a matrix to wrap - defaults to an empty matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    list(
    	set = function(y) {
	            x <<- y
	            m <<- NULL
		    }, 
    	get = function() x,
        setInverse = function(inverse) m <<- inverse,
        getInverse = function() m
    )
}


## cacheSolve
## Given a 'matrix' as returned by makeCacheMatrix, this function
## will return the inverse of that 'matrix'.
## x   -> the 'matrix' returned by makeCacheMatrix
## ... -> addtional arguments to the 'solve' function that caculates
##        the inverse

cacheSolve <- function(x, ...) {
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
