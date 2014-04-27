## makeCacheMatrix and cacheSolve accept a matrix and compute its 
## inverse. If the inverse already exists, the functions return 
## the cached value rather than re-computing it.

## makeCacheMatrix defines the functions that are used to set and 
## get the values of the original matrix and its inverse.

makeCacheMatrix <- function(matrixData = matrix()) {
    
    ##ADD COMMENT
    invMatrix <- NULL
    
    ## set function to create the cached matrixData
    setMatrix <- function(y) {
        matrixData <<- y
        invMatrix <<- NULL
    }
    
    ## get function to return the cached matrixData
    getMatrix <- function() matrixData
    
    ## set function to create the cached inverted matrix
    setInvMatrix <- function(i) invMatrix <<- i
    
    ## get function to return the cached inverted matrix
    getInvMatrix <- function() invMatrix
    
    ## create the list of functions to return 
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
  
}


## Determine the inverted value of the matrix; 
## if the inverted matrix exists, then pull from cache

cacheSolve <- function(mObj, ...) {
    
    ## try to get the inverted matrix
    invMatrix <- mObj$getInvMatrix()
    
    ## Check to see if the inverted matrix exists, and if it does, 
    ## return it.
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    ## Compute the inverted matrix, if it doesn't exist.
    matrixData <- mObj$getMatrix()
    invMatrix <- solve(matrixData, ...)
    
    ## Set the new inverted matrix to the environment.
    mObj$setInvMatrix(invMatrix)
    invMatrix
  
}
