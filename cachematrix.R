## Two functions here work in tandem to invert an invertible matrix

## Writes up a list that covers both cases
## namely when the inverse is cached and when it's not

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Returns a matrix that is the inverse of x
## after first taking care to avoid the computation if unnecessary

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cashed data")
		return(inv)
	}
	data <-x$get()
	inv<- solve(data, ...)
	x$setinverse(inv)
	inv
}
