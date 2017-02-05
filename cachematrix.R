# Written by Aleksander Jarosz
# 5.2.2017

# Function returns special matrix "object" with caching functionality
# Supplied parameter of type matrix
makeCacheMatrix <- function(x = matrix()) {
	# inv holds the inverse of the matrix. Start with an empty one
    	inv <- NULL
    
    	# Define four methods that can be used on the special matrix object
    	# Sets value of the function when matrix changes
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    	# Returns the original matrix
    	get <- function() x
    	# Sets the inverse of the matrix
    	setinv <- function(i) inv <<- i
    	# Returns the inverse of the matrix
    	getinv <- function() inv
    	# Passes the variables
    	list(set = set, get = get,
        	 setinv = setinv,
         	getinv = getinv)
}

# Calculates the inverse of the matrix via solve or uses cached, if matrix did not change

cacheSolve <- function(matrixobject, ...) {
    # Get the inverse from the cache
    i <- matrixobject$getinv()
    # Check if the inverse exists. If yes, simply return it (and be done with work)
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # If we arrived here, i was null
    # Get the original matrix from the matrix object
    data <- matrixobject$get()
    # Calculate the inverse of the matrix
    i <- solve(data, ...)
    # Write that inverse to the matrixobject to re-use later (so essentially, cache it)
    matrixobject$setinv(i)
    # Return it
    i
}
