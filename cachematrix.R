## This function creates a special "matrix" object that can cache its inverse to reduce processing resources
## by allowing the matrix inversion to be reused instead of being recalculated

## The makeCacheMatrix function uses the R solve() function to invert a square and save the results in the global environment
## if the matrix has already been cached and save it is not solved again

makeCacheMatrix <- function(x = maxtrix()) {
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setinv <- function(solve) m <<- solv
	getinv <- function() m

	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)


}


## The cacheSolve() function works with the makeCacheMatrix() function to reduce resources required to solve a square
## matrix

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
