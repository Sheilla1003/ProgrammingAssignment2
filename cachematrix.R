##Coursera R Programming Assignment #2
## 02 December 2017
## author: sheillareyes1003@gmail.com


## Caching the Inverse of a Matrix
## Below are a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        
        set <- function(y) {                
                x <<- y                   
                inv <<- NULL              
        }
        get<- function() x                
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv                    
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
        
}


## This function computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix function. It first checks to see if the inverse has already
##been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in te cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message ("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
