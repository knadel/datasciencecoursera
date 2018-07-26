## Put comments here that give an overall description of what your
## functions do:

## Return a list containing functions:
## 1) to set, 
## 2) to get, 
## 3) to set inverse and 
## 4) to get inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function
## solving for the inverse of the original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## calculates inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## sets inverse value in cache
    x$set_inverse(inv)
    
    inv
}
