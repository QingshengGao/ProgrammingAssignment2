## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a single argument, which is the initial value of the matrix
## It returns a list with four functions as its elements. The four elemets are:
## set - a function to reset the matrix
## get - a function to get the matrix
## setInverse - a function to cache inverse of the matrix
## getInverse - a function to retrieve the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # the cached inverse of the matrix, initialized to NULL
		inv <- NULL
        
		# a function to reset the matrix, and reset the inverse to NULL
		set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		# a function to retrieve the matrix
        get <- function() x
		
		# a function to cache the inverse of the matrix. 
        setInverse <- function(inverse) inv <<- inverse
		
		# a function to retrieve the inverse of the matrix
        getInverse <- function() inv
        
		# Wrap the four function in a list as the result returned.
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## It takes a single argument, which is a special "matrix" returned by makeCacheMatrix above
## It returns the inverse of the matrix
cacheSolve <- function(x) {
        # retrieve the inverse of the matrix
		inv <- x$getInverse()
        
		# if the inverse exists, return it; otherwise continue
		if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
		# retrieve the underlying matrix
		theMatrix <- x$get()
        
		# calculate the inverse of the matrix
		inv <- solve(theMatrix)
        
		# cache the inverse
		x$setInverse(inv)
        
		# return the inverse
		inv
}
