## This set of functions sets a cached matrix and calculates the correspondent inverse matrix

## This function caches a Matrix and its attributes:
## set import a matrix
## get returns a imported matrix
## setdet import the determinant value
## getdet return the cached determinant velue
## setinv import inverse matrix
## getinv returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		detm <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                detm <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        setdet <- function(detm) detm <<- det
        getinv <- function() inv
        getdet <- function() detm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,setdet = setdet, getdet = getdet )
}


## This function calculate the inverse matrix
## the function returns the cached inverse matrix in case it has been already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        
        detm <- x$getdet()
        
        if(!is.null(detm) && detm != 0) {
                inv <- solve(data,...)
                x$setinv(inv)
                return(inv)
        }

        detm <- det(data, ...)
        x$setdet(detm)

        if(detm != 0){

        	inv <- solve(data,...)
        	x$setinv(inv)
        	return(inv)
        }

        message("Matrix is not invertible")
        return(NULL)
}

