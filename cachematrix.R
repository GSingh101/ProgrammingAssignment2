## Compute the inverse of a matrix and cache the result so that if the inverse
## is required in the future it can be looked up

## Create and return a list which contains various functions

makeCacheMatrix <- function(x = matrix()) {
       ## Set the inverse to be NULL initially
       inverse <- NULL
       
       ## Create a function to set the matrix x to be inverted
       setmatrix <- function(y) {
               x <<- y
               inverse <<- NULL
       }
       
       ## Create a function to get the matrix x to be inverted
       getmatrix <- function() {x}
       
       ## Create a function to set the inverse of the matrix x
       setinverse <- function(inv) {inverse <<- inv}
       
       ## Create a function to get the inverse of the matrix x
       getinverse <- function() {inverse}
       
       ## Create and return the list of functions defined above
       list(setmatrix = setmatrix, getmatrix = getmatrix, 
            setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix if it does not already exist

cacheSolve <- function(x, ...) {
        ## Get and return the inverse of the matrix if it already exists
       inverse <- x$getinverse()
       if (!is.null(inverse)) {
               inverse
       }
       
       ## Otherwise get the matrix and compute it's inverse 
       matrixdata <- x$getmatrix()
       inverse <- solve(matrixdata, ...)
       x$setinverse(inverse)
       inverse
}
