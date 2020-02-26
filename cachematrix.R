## #Programming Assignment: Catching the inverse of a Matrix
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.

## This functions will set and get the value of the matrix and then will do the same
# with the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { #Setting the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x #Getting the value of the matrix
        setinverse <- function(inverse) i <<- inverse #Setting the inverse of the matrix
        getinverse <- function() i #Getting the inverse of the matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The following function calculates the inverse of the matrix returned by the previous
#function. 
#It will first check if the inverse has already been calculates. Ih thats the case, it will get
#the inverse from the cache and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse() #check the inverse
        if (!is.null(i)) {
                message("getting cached data") #in case there is already an inverse it will get it from the cache
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

#If we use the function solve(#square matrix we want to evaluate#), we will get the
# same result that we get with the use of the previous function.

