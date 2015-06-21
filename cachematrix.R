## The functions cache and solve the Inverse of a Matrix.
## They can only solve invertible square matrix.

## makeCacheMatrix function stores values of input matrix and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInvMat <- function(invMat) im <<- invMat
    getInvMat <- function() im
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


## cacheSolve function computes the inverse of the input matrix which is stored 
## makeCacheMatrix function. If the inverse matrix has already been calculated
## and the input matrix has not been changed, the cacheSolve function will get
## the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    im <- x$getInvMat()
    if(!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInvMat(im)
    im
}

## Screenshot of example outputs can be found at
## 
