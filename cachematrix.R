## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() creates a “matrix” object that caches the matrix and 
## the inverse of the matrix

makeCacheMatrix <- function(z = matrix()) {
    set <- function(y) {
        # Cache the matrix and the matrix inverse
        z <<- y
        zinverse <<- solve(y)
    }
    get <- function() z #return the cached matrix
    getinverse <- function() zinverse  #return the inverse of the cached matrix
    list(set = set, get = get, getinverse = getinverse)
}

## cacheSolve() returns the inverse of a matrix. it will only compute
## the inverse if the cached matrix is different, otherwise willl get it
## from the cache.

cacheSolve <- function(g, x, ...) {
    ## Return a matrix that is the inverse of 'x' using matrix object g 
    
    ## Get the cached matrix and see if it has changed
    ## if the matrix is the same, return the cached inverse
    matrix <- g$get()
    if (all.equal(matrix,x)) {    #matrix is the same, retrieve the cached inverse
        imatrix <- g$getinverse()
    }
    else {    # the matrix is different, calculate the inverse
        imatrix <- solve(x)
    }
    imatrix #return the inverse of 'x'
}
