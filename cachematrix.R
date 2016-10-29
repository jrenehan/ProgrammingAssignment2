## The purpose of this assignment is to write a pair of functions that
## cache the inverse of a matrix while avoiding a costly computation process.

## The makeCacheMatrix function computes the inverse of the matrix. It
## creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {	
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve function returns the inverse of the matrix. It begins 
## by checking if the inverse has already been computed. If it has then 
## it retrieves the inverse from the cache and skips the computation. 
## If the inverse has not already been computed, then it carries out the 
## computation of the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
