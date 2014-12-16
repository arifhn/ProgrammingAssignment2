
## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## initialize cache variable to NULL
	cache <- NULL
	## define set function
	set <- function(y) {
		## replace matrix value and reset cache if matrix content changed
		if(!(is.matrix(y) && dim(x) == dim(y) && all(x == y))) {
			# save new value
			x <<- y
			## reset cache
			cache <<- NULL
		}
	}
	## define get function (return matrix value)
	get <- function() x
	## define setinverse function (set the cache)
	setinverse <- function(inverse) cache <<- inverse
	## define getinferse function (return the cache)
	getinverse <- function() cache
	## return defined functions as list
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of special matrix returned by makeCacheMatrix with simple cache
cacheSolve <- function(x, ...) {
	## check cached 'inverse' value
	i <- x$getinverse()
	if(!is.null(i)) {
		# if the cache value is not null
		message("return cached inverse") # print debug message
		# return the cached value
		return(i)
	}
	# read matrix data and store it to data variable
	data <- x$get()
	# call function solve to calculate the inverse
	i <- solve(data, ...)
	# store calculated value to our cacheMatrix
	x$setinverse(i)
	# return inverse value
	i
}
