## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions that will work on the argument x.
## Argument x will be initiated when this function is called. The four 
## functions are set, get, setinv and getinv. The cache for this function is
## the variable inv.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list( set = set, get = get,
			setinv = setinv,
			getinv = getinv)

}


## This function actually calculates the inverse of the input matrix. If
## the inverse of the matrix is already available in the cache, it retrieves
## it, or else, it calculates it afresh and then returns it.


cacheSolve <- function(x, ...) {
         inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
