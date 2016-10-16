## Matrix inversion is a costly computation, so it is better to cache
## the inverse of a matrix than compute it repeatedly.

## The first function, makeCacheMatrix, creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This second function calculates the inverse of matrix above.
## However, it first checks whether the inverse is already calculated.
## If yes, it skips computation and just get the inverse
## from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
		}
		mat <- x$get()
		inv <- solve(mat, ...)
		x$setInverse(inv)
		inv
	}
}
