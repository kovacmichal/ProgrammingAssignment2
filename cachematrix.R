## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix ## inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## @param x - matrix
## @return - list of functions:
##		- set value of matrix
##		- get value of matrix
##		- set value of inverse matrix
##		- set value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	invX <- NULL
	setMatrix <- function(y) {
		x <<- y
		invX <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(inverse) invX <<- inverse
	getInverse <- function() invX
	list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse ,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## @param x - result object of makeCacheMatrix function call
## @return - for first call return input matrix, next call return cached matrix

cacheSolve <- function(x, ...) {
	invX <- x$getInverse()
	if (!is.null(invX)) {
		message("Loading matrix from cache")
		return(invX)
	}
	data <- x$getMatrix()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
