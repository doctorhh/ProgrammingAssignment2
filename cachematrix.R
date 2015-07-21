# The makeCacheMatrix function creates and returns a list of  4 functions
# used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
      # Set the value of inv as NULL. Default value if the cacheSolve was not used before
      inv <- NULL
      
      # Function to set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # Function to return the input matrix
      get <- function() x
      # Function to set the inv to the inversed matrix - with the solve() function
      setInverse <- function(inverse) inv <<- inverse
      # Function to return the inversed matrix
      getInverse <- function() inv
      # Set a list of function to be used as x$function
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
      # Function to get the inversed matrix value
	inv <- x$getInverse()
	# Return the value the inversed martix-value if not NULL: 
	
	if(!is.null(inv)) {
      	message("getting cached data.")
      	return(inv)
	}
	
	# Get the value from the matrix
      cache_dat <- x$get()
      # Compute the inverse of the matrix value
      inv <- solve(cache_dat)
      # Function to set the value of the cache object.
      x$setInverse(inv)
      inv
}