
makeCacheMatrix <- function(x = matrix()) {
	## X is assumed to be a square invertible matrix
	## A list is returned that has functions to set/get the matrix, and
	## set/get the inverse.
	## The list is the input to cacheSolve()

	inv <- NULL
	set <- function(y) {
		x <<- y    ## assigns value to obj in env other than current
		inv <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()

	## See if the inverse has already been done
	if (!is.null(inv)) {
		## if inv is not null then the inv has been calc'd and can be grabbed
		## from the cache; additional computation is skipped
		message("getting cached data")
		return(inv)
	}

	## If not cached, the inverse is now calculated
	data <- x$get()    ## get the matrix data
	inv <- solve(data, ...)    ## calculate the inverse
	x$setinverse(inv)    ## set the matrix inverse in the cache

	inv    ## return the inverse
}
