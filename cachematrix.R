## Creates a list of accessor / mutator functions for storing
## a matrix, calculating its inverse, its caching the inverse

## Returns list of accessor/mutator methods (get/set) for the matix and its inverse

makeCacheMatrix <- function(x = matrix() ) {
    xInv <- NULL
    set <- function(y) {  ##may need to set argument as a matrix
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) xInv <<- inv
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Returns matrix inverse from solve calculation or from cache

cacheSolve <- function(x, ...) {
    xInv <- x$getInv()
    if(!is.null(xInv) ) {
          message("Getting cached data...")
          return(xInv)
    }
    xMatrix <- x$get()
    xInv <- solve(xMatrix, ...)
    x$setInv(xInv)
    xInv
        ## Return a matrix that is the inverse of 'x'
}
