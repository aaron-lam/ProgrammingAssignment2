## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse matrix to NULL
        inv <- NULL

        # Define a function to set the matrix
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL # Reset the cached inverse when the matrix is updated
        }

        # Define a function to get the matrix
        get <- function() x

        # Define a function to set the inverse
        setinverse <- function(inverse) inv <<- inverse

        # Define a function to get the inverse
        getinverse <- function() inv

        # Return a list of functions
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Get the cached inverse
        inv <- x$getinverse()

        # If the inverse is already calculated, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # If the inverse is not cached, calculate it using solve
        data <- x$get()
        inv <- solve(data, ...)

        # Cache the calculated inverse
        x$setinverse(inv)

        # Return the inverse
        inv
}
