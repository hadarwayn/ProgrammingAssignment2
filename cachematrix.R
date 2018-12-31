## Coursera R Programming course - Programming Assignment 2
## Goal: Cache the inverse of a matrix



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## (It has been created based on the makeVector function given in the assignment)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## (It has been created based on the cachemean function given in the assignment)

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()   ## Return a matrix that is the inverse of 'x'
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Else: Since no cached Matrix, setting a new inverse for the Matrix 'x'
    matrix <- x$get()
    inv <- solve(matrix)   ## The solve(x) returns the inverse of X, if x is a square invertible matrix.
    x$set_inverse(inv)
    inv
}
