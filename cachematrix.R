## Below are two functions that are used to creat a special
## "matrix" object that stores the matrix and caches its inverse

## makeCacheMatrix creates a special "matrix" objects, which has a
## list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix created
## with makeCacheMatrix. First it checks to see if the inverse has
## has been already computed. If yes, it gets the inverse using
## "getinverse" and skips the "solve". If not, it computes the
## inverse using "solve" function and store the inverse using
## "setinverse" in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

