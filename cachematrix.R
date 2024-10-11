## We're going to create two functions in order to compute the inverse of a matrix.
## If it's been calculated previously, then it's taken from cache

## This function creates a special "matrix" object that can cache its inverse.

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

## This function calculates the inverse of the special "matrix". If it's been already calculates,
## it takes the inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()  
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setInverse(inv)
        
        inv
}

