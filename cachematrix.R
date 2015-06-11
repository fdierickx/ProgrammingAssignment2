## R Programming: Programming Assignment 2 (Lexical Scoping)
## 
## Example usage:
## x <- rbind(c(1, -1/4), c(-1/4, 1))
## y <- makeCacheMatrix(x)
## cacheSolve(y)     ## first attempt, caching not available
## cacheSolve(y)     ## subsequent attempt, caching available


## makeCacheMatrix() is a function that takes a matrix as argument and returns
## a list of functions available for later use with the cacheSolve() function
## described below.

makeCacheMatrix <- function(x = matrix()) {
	## Set the variable 'inv' to NULL
	inv <- NULL

	## The set function uses the global environment to store two variables
	## for later reuse. The argument 'y' is assigned to variable 'x' while
	## the 'inv' variable is set to NULL in order to clear any previously
	## stored values for this variable.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## The get function returns the 'x' variable from the global environment
	## (as set by the above set function)
	get <- function() x

	## The setInverse function assigns the argument 'inverse' to the 'inv' 
	## variable stored in the global environment. This will make it
	## available for later reuse (caching functionality).
	setInverse <- function(inverse) inv <<- inverse

	## The getInverse function returns the 'inv' variable from the global
	## environment.
	getInverse <- function() inv

	## To store the above functions in the object which we assign the 
	## makeCacheMatrix() function to, we use the list() function. This
	## will make sure the object has access to all of these functions.
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function utilizes the global environment for caching reasons.
## This should fasten up the solving of a matrix that was previously calculated.
## If no caching is available, the matrix will be solved and stored in the 
## global variable for caching purposes. Any subsequent call will use the
## value stored in the cache and thus avoid recalculation by every function call.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()

	## If the result in the 'inv' variable is not null, a solved matrix
	## was available in the global environment and can be used for 
	## caching purposes.
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	## In case no caching was available, we will solve the matrix using the
	## R code below and attempt to store it in the global environment for
	## caching purposes.

	## First, we retrieve the matrix from the 'x' argument
	data <- x$get()

	## Secondly, we'll use the solve the matrix in the 'data' variable
	## using the solve() function.
	inv <- solve(data, ...)

	## Finally, we use the setInverse() function to store the solved matrix
	## in the global environment for caching purposes.
	x$setInverse(inv)

	## After the solving and caching of the result is done, we return the
	## calculated inverse.
	inv
}