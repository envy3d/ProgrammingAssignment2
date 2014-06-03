## Only solves the inverse matrix when the matrix has been modified.

## Create a 4 function Cached Matrix "object" by passing in
## a square invertible matrix.

makeCacheMatrix <- function(cMatrix = matrix()) {
    invMatrix <- NULL
    
    set <- function(newMatrix) {
        cMatrix <<- newMatrix
        invMatrix <<- NULL
    }
    get <- function() cMatrix
    setInverse <- function(newInvMatrix) invMatrix <<- newInvMatrix
    getInverse <- function() invMatrix
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse matrix
## Takes a makeCacheMatrix "object". Only resolves the inverse
## matrix when the matrix has been modified.

cacheSolve <- function(cMatrix, ...) {
    invMatrix <- cMatrix$getInverse()
    
    if (!is.null(invMatrix)) {
        message("getting cached inverse matrix")
        return(invMatrix)
    }
    mat <- cMatrix$get()
    invMatrix <- solve(mat)
    cMatrix$setInverse(invMatrix)
    invMatrix
}

