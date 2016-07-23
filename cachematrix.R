#####################################################################################
# Title:    makeCacheMatrix, cacheSolve                                             #
# PUrpose:  makeCacheMatrix - set of functions to get and set the matrix and it's   #
#                             inverse.                                              #
#           cacheSolve - Determines if the inverse of a submitted needs to be       #
#                        computed.  This determination is based on the matrix       #
#                        submitted.  If the matrix has not changed, the previously  #
#                        computed inversed is returned. If the matrix did change,   #
#                        the inverse of the new matrix is computed and returned.    #
# Author:                                                                           #
# Date:     2016.07.09 - Initial Build                                              #
#           2016.07.13 - More building                                              #
#####################################################################################


makeCacheMatrix <- function(x = matrix()) {
    # cached inverse of matrix
    inv <- NULL
    
    ## getter/setter for matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## getter/setter for matrix inverse
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    
    ## return list of functions for matrix
    list(get=get, set=set, getinv=getinv, setinv=setinv)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # return cached matrix inverse if it's been already computed
    if (!is.null(inv)) {
        message("inverse is cached")
        return(inv)
    }
    
    # compute inverse of matrix 
    m <- x$get()
    inv <- solve(m, ...)
    
    # cache inverse
    x$setinv(inv)
    
    # return inverse of matrix
    return(inv)
}


