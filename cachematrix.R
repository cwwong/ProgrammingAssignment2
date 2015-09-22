## Put comments here that give an overall description of what your
## functions do


## These two functions help us to store the inverse of a matrix into cache after it is calculated. If later 
## on, we need it again. It will look up and read the inverse out from cache if the matrix has not been changed, otherwise
## we calculate it again

## Write a short comment describing this function
## The function makeCacheMatrix create an object in which we can 1)set the data, 
## 2)get the data, 3) set the inverse and 4) get the inverse
## It is important to know that in the set function, the matrix will only be set if it is a difference one

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		if(!identical(x,y)){
			x <<- y
			m <<- NULL
		}		
	}
	
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function
## The function expect an makecacheMatrix object as input. It will return the inverse of the matrix if
## 1) the inverse has already existed, it will read it out from cache or,
## 2) the inverse has NOT been calculated, it will find and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse ()
	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
