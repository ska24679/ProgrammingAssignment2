##Two methods:
## makeCacheMatrix takes a square invertible matrix as an argument,
## and returns a list with single methods to act as setters and getters
##
## cacheSolve takes a list created by makeCacheMatrix, and returns the
## inverse of the matrix passed to makeCacheMatrix.  It will only
## calculate the inverse once.

##makeCacheMatrix takes a matrix, and returns a list of
##of methods that set and get a matrix, and its inverse,
##within the scope of the generated list only.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set=set,
		get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


##cacheSolve checks to see if there is an inverse in the scope
##of the list, and calculates it only if it doesn't exist.
##Then it returns the inverse
cacheSolve <- function(x, ...) {
	inv=x$getinverse()
	if(is.null(inv)) {
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
	} else {
		message("getting cached data")
	}
	inv
}
