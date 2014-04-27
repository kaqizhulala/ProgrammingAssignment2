## This R function is to cache time-consuming computation: matrix inversion.
## Here we take advantage of the scoping rules of R and try to preserve state 
## inside of an R object.
## This functions includes the following two functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

## The first function, makeCacheMatrix creates a special "vector", which is 
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse matrix
    setInv <- function(inverse) inv <<- inverse
    ## get the value of the inverse matrix
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The following function calculates the inversion of the special "vector" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    ## first check the cache
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    ## if not in the cache, compute the inverse
    myMatrix <- x$get()
    inv <- solve(myMatrix, ...)
    x$setInv(inv)
    inv    
}
