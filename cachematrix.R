## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      ## initialize cache
      cache <- NULL
      ## define set function
      set <- function(y) {
            ## replace matrix value and reset cache if content changed
            if(!(is.matrix(y) && dim(x) == dim(y) && all(x == y))) {
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
    ## check cached 'inverse'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("return cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
