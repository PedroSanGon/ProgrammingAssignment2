## Get the inverse of a square matrix avoiding repeated calculations
##
## Given a square matrix, x
##
## 1- L<-makeCacheMatrix(x) --> auxiliar list to store cache copies of
## the matrix and its inverse
##
## 2- cacheSolve(L) --> inverse of x. It is repeatedly called, it get the 
## chached copies, withoud reapiting calculations

## Auxliar function to store cache copies of the matrix and its inverse
##
## From a matrix to 
## (set matrix,get matrix,set inverse matrix,get inverse matrix)
##
## get --> only obtained one time per matrix
makeCacheMatrix <- function(x = matrix()) {
	  ## fields for the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	  ## fields for the inverse
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

	  ##return
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of a matrix from a list of cached copies
##
## First time --> invokes solve
## Rest of the calls --> get chached copies
cacheSolve <- function(x, ...) {
        ## Check if it is already computed
        m <- x$getinverse()

	  ## Get from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	  ## Compute in other case
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
