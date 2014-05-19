## These two functions create a matrix capable of caching its inverse,
## calculate the inverse, and then store that inverse with the original
## matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The function returns a list containing functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the matrix inverse
##  4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" created by the function 
## above using the R solve function. If the inverse has already been calculated
## and the matrix has not changed, then the inverse is retrieved from cache and
## not recalculated.
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
