## Functions for assignment 2, Coursera rprog-004
##
## Pair of functions that cache the inverse of a matrix.
## NB: the matrix should be invertible




## makeCacheMatrix creates a list containing functions to
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_x <<- solve
        getinv <- function() inv_x
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## cacheSolve calculates the inverse of the matrix that 
## was set with makeCacheMatrix. 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. 

cacheSolve <- function(x,...) { 
        
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        data <- x$get()
        inv_x <- solve(data)
        x$setinv(inv_x)
        
        inv_x
}
