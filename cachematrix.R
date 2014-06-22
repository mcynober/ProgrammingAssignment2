## Matrix inversion is a costly computation. The following pair of functions will cache the
## inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
	set <- function(y) {
		x <<- y
		inverse_x <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverse_x <<- inverse
	getinverse <- function() inverse_x
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inverse_x <- x$getinverse()
	if(!is.null(inverse_x)) {
		message("getting cached inverse matrix")
		return(inverse_x)
	} else {
		data <- x$get()
		inverse_x <- solve(data, ...)
		x$setinverse(inverse_x)
		return(inverse_x)
	}
}
