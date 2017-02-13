## Put comments here that give an overall description of what your
## functions do

-## function makeCacheMatrix is creating an environment that keeps functions, matrix and its inverse, 
## result of makeCacheMatrix list of functions that allow manipulation on the matrix and its Inverse
## Write a short comment describing this function
        
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
        x <<- y       ## this line allows modifing matrix x in the parental environment
        inv <<- NULL  ## this is needed if we modify matrix we need to set cashed inversion to NULL
        }
        
        get <- function() x                     ## anonymous function to get matrix 
        setinv <- function(solve) inv <<- solve ## function that as argument is using another function solve to set inv in paretnal environment
        getinv <- function() inv                ## anonymous function to get inversion of matrix
        
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
        
        }
        
        
## Function cacheSolve is either calculating Inversion of new matrix or getting cached version of inversion
## Return a matrix that is the inverse of 'x'
                
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()   
        ## first we need to check if inversion is cached or not, it is not cached if we have new matrix x
        if (!is.null(inv)) {
                message("Getting cached Inverted Matrix")
                return (inv)
                }
        k <- x$get()
        inv <- solve(k, ...)
        x$setinv(inv)
        inv           ## Returning a matrix that is the inverse of 'x'
}
