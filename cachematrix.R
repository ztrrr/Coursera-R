## Put comments here that give an overall description of what your
## functions do:

## These functions take a matrix and cache it and its inverse in a vector so that if 
## the matrix inversion is already calculated it will return the cached values instead of re-running the 
## matrix inversion calculations.

## Write a short comment describing this function:

## This function creates a vector of the matrix and its inverse using the list function.
## "i" functions as a flag that tells the cacheSolve function if the matrix inversion has already been performed
## The "set" function stores the incoming matrix
## The "get" function returns the matrix
## The "setinv" function stores the inverted matrix
## The "getinv" function returns the stored inverted matrix
## List creates a list of the function "set", "get", "setinv" and "getinv"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                y <<- x
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) {
                i <<- inv
        }
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function:

## This function performs the matrix inversion calculation, or, if the inversion has already been performed,
## will print the message "retrieving from cache" and return the cached matrix.

cacheSolve <- function(x, ...) {
        if(!is.null(i)) {
                message("retrieving from cache")
                return (i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        
        ## Return a matrix that is the inverse of 'x':
        return(i)
}
