## Short description of the complete function
## makeCacheMatrix takes a function and stores it in Cache
## It also creates a inverse of the matrix and sets it to a default value of NULL
## makeCacheMatrix also initializes functions to retrieve the input Matrix from...
## ... cache to calculate its inverse of it has not already been calculated

## Short description of makeCacheMatrix
## The input for makeCacheMatrix is assumed to be an invertible matrix
## makeCacheMatrix initializes an Inverse for input matrix and sets it to NULL
## Then the input matrix and the inverse are stored in cache
## Lastly, functions are defined to retrieve the input matrix and its inverse from cache
## Also, a function is defined to store the inverse of the matrix (if NULL is in cache)

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL
	set <- function(y) {
		x <<- y
		Inv <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) Inv <<- solve
	getmatrix <- function() Inv
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

## Short description of cacheSolve
## cacheSolve takes as an input the matrix cached in makeCacheMatrix
## The inverse of the cached matrix is retrieved from the cache
## If the inverse is not a NULL, cache matrix returns the inverse value stored in cache...
## ... and displays a message that inverse was already stored in the cache and did not...
## ... have to be recalculated
## If the inverse value in the cahce is a NULL, cacheSolve calculates the inverse using...
## ... the built-in in R function "solve"
## The calculated inverse is then stored to cache and also returned to the function...
## ... that called cacheSolve

cacheSolve <- function(x=matrix(), ...) {
	Inv <- x$getmatrix()
	if(!is.null(Inv)) {
		message("getting cached data")
		return(Inv)
	}
	data <- x$get()
 	Inv <- solve(data, ...)
	x$setmatrix(Inv)
	Inv
}



