## #Programming Assignment: Catching the inverse of a Matrix
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
# We can take the example with the Vector and apply it to a matrix
## This functions will create a matrix that is really a list containing a function
# to set and get the value of the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { #Setting the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x #Getting the value of the matrix
        setinverse <- function(inverse) m <<- inverse #Setting the inverse of the matrix
        getinverse <- function() m #Getting the inverse of the matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The following function calculates the inverse of the matrix returned by the previous
#function. 
#It will first check if the inverse has already been calculated. If thats the case, it will get
#the inverse and sets the value of the inverse in the cache using the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse() #check the inverse
        if (!is.null(m)) {
                message("getting cached data") #in case there is already an inverse it will get it from the cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#If we use the function solve(#square matrix we want to evaluate#), we will get the
# same result that we get with the use of the previous function.

