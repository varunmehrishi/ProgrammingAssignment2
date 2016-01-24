## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, 
	     setinv = setinv,
	     getinv = getinv)
}


## Write a short comment describing this function
## If the special matrix has cached inverse returns the cached inverse
## Otherwise finds the inverse using solve and caches the inverse to the special matrix
cacheSolve <- function(x, ...) {
	inv <- x$getinv()	## Return a matrix that is the inverse of 'x'
	if(!is.null(inv)) {
		message("getting cached inverse")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv	
}
