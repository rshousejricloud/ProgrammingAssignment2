## This code will cache the results of an inverse() operation on a matrix

## This creates a list containing a function to set and get the value of a 
## matrix, as well as to set and get the value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix<- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix<<- inverse
    getinverse <- function() inverse_matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix after checking to see if 
# the inverse was already created and cached in a prior invocation. If not, it computes
# the inverse and sets the value in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
    inverse_matrix<- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data.")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix<- solve(data)
    x$setinverse(inverse_matrix)
    inverse_matrix
}