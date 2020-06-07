## Create a matrix object which cache its inverse
makeCacheMatrix <- function(x=matrix()){
        ## Initialize the inverse 
        inv <- NULL
        ## Set the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## Get the matrix
        get <- function(){x}
        ## Set the inverse of the matrix
        setInverse <- function(inverse){inv <<- inverse}
        ## Get the inverse of the matrix
        getInverse <- function() {inv}
        ## Return a list of the methods
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix". 
cacheSolve <- function(x, ...){
        ## Return a inverse matrix of 'x'
        inv <- x$getInverse()
        ## Just return the inverse if its already present
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Get the matrix
        mat <- x$get()
        ## Calculate the inverse
        inv <- solve(mat, ...)
        ## Set the inverse
        x$setInverse(inv)
        ## Return
        inv
}

