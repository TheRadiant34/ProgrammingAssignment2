## Matrix inversion is usually a costly computation and there maybe some
## some benefit to caching the inverse matrix rather than computing it
## repeatedly. The following functions are used to cache the inverse of
## a matrix

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {            
                x <<- y
                inverse <<- NULL
        }
        get <- function() x              
        setinv_matrix <- function(inv_matrix) inverse <<- inv_matrix
        getinv_matrix <- function() inverse
        list(set = set, get = get,
             setinv_matrix = setinv_matrix,
             getinv_matrix = getinv_matrix)
       
}

## The cahceSolve function returns the inverse of the matrix. 
## First it checks if there is already an inverse value. If so, it "gets" the 
## result and skips the computation. If there is no inverse value or the value
## changed, it computes the inverse, sets the value in the cache via the
## setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv_matrix()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)     
        x$setinv_matrix(inverse)
        inverse
}





