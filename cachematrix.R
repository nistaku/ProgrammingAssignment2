## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize cache for the inverse
    
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset cached inverse when the matrix is changed
    }
    
    get <- function() x  # Retrieve the original matrix
    
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    
    getInverse <- function() inv  # Retrieve the cached inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been computed, it retrieves the cached value.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    mat <- x$get()  # Retrieve the original matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    
    inv
}