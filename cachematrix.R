## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse to save computing time.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
        x <<- y
        m <<- NULL
     } ## This is for change the original matrix
     get <- function() x ## get the matrix to be inversed from the function
     setinv <- function(solve) m <<- solve ## set the function to inverse matrix
     getinv <- function() m  ## get inverse matrix by calling m
     list(set=set, get=get, setinv=setinv, getinv=getinv) 
     ## the function actually returns a list with 4 sub functions.
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)){
       message("getting cached data")
       return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinv(m)
     m
}
