## Put comments here that give an overall description of what your 
## functions do

## makeCacheMatrix is filled with set, get, setInverse and getInverse
## library(MASS) can be used to set the inverse of a non-squared as well as 
## squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL           ## initializing inverse as NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}  ## function to get matrix x
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function(){
                inv <- ginv(x)
                inv%*%x     ## function to obtain inverse of the matrix
                }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This function can be used to get the cache data, so we can use our time in a 
## more efficient way

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv 
}