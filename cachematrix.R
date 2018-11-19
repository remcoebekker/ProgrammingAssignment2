## These functions help speed up repeated access to an inverted matrix. 
## First you use the makeCacheMatrix function to create a container for a matrix and its inversion.
## After having set a to be inverted matrix to the container, the cacheSolve function can be used to 
## determine the inverson of the matrix or to return it immediately from cahce if it has already been determined.

## makeCacheMatrix creates the container for a matrix and its inversion. You can either supply a (filled)
## matrix to it or set it later using the set() function. 
makeCacheMatrix <- function(x = matrix()) {
        ## Set-up the cache to hold the inverted matrix.
        cachedInvertedMatrix <- NULL

        ## Function to set the to be inverted matrix.        
        set <- function(matrix) {
                  x <<- matrix
                  cachedInvertedMatrix <<- NULL
        }
        
        ## Function to get the to be inverted matrix.        
        get <- function() x
        
        ## Function to cache the inverted matrix.      
        setInverted <- function(invertedMatrix) cachedInvertedMatrix <<- invertedMatrix
        
        ## Function to get the inverted matrix from cahce.      
        getInverted <- function() cachedInvertedMatrix
        
        ## Return a list with the four 4 functions that make up a cacheMatrix.
        list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## cacheSolve can be used to determine the inversion on a list created by the makeCacheMatrix function.
## If the inversion has already been deterimed this inversion is returned from cache. 
cacheSolve <- function(x, ...) {
        ## Get the inverted matrix from cache.
        invertedMatrix <- x$getInverted()

        ## Check whether the inverted matrix is present in the cache.
        if (!is.null(invertedMatrix)) {
                ## The inverted matrix is present in the cache, so we return it.
                message("getting cached data!")
                return(invertedMatrix)
        }

        ## The inverted matrix is not present in the cache, so we create it, cache it and return it.
        matrix <- x$get()
        invertedMatrix <- solve(matrix, ...)
        x$setInverted(invertedMatrix)
        invertedMatrix
}