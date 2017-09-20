## Caching the Inverse of a Matrix

## makeCacheMatrix function
# This function creates a special "matrix" object that can cache its inverse, 
# which is really a list containing functions to
# 	1. set the value of the matrix
# 	2. get the value of the matrix
# 	3. set the value of the inverse
# 	4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set=set, get=get,
		setinv=setinv,
		getinv=getinv)
}

## cacheSolve function
# This function computes the inverse of the 'matrix' object returned by makeCacheMatrix.  
# If the inverse has already been calculated, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

## Testing 
#	> mx<-matrix(c(4,2,7,6),2,2)
# 	> cmx<-makeCacheMatrix(mx)
#	> cacheSolve(cmx)
#	     [,1] [,2]
#	[1,]  0.6 -0.7
#	[2,] -0.2  0.4
#	> cacheSolve(cmx)
#	Getting cached inverse
#	     [,1] [,2]
#	[1,]  0.6 -0.7
#	[2,] -0.2  0.4
