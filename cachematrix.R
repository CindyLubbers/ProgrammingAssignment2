## Below are two functions, makeCacheMatrix and cacheSolve, that are 
## used to create a special object that stores a numeric matrix and 
## caches its inverse.

## The following function creates a special "matrix", which is really a
## list containing a function to
## - set which sets the value of the matrix
## - get which gets the value of the matrix
## - setinv which sets the value of the inverse of the matrix
## - getinv which sets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function, makeCacheMatrix. However, it first 
## checks to see if the inverse has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function.
## If you wish to pass arguments into the solve function, you can do so
## after the argument x (in stead of the dots)
## If the function gets the inverse from the cache, it tells you so. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}