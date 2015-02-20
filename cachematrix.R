## The following functions create a special object that stores a matrix and  
## caches the inverse of that matrix

## Creates a "special list" with a set of functions: set, get, setInverse and
## getInverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Calculates the inverse of the matrix contained in the "special list"

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}