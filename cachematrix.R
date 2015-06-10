## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse to save computing time.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     ## Set the value of the vector. This is for change the original matrix.
     set <- function(y){
        x <<- y
        m <<- NULL
     } 
     ## Get the value of the matrix
     get <- function() x 
     ## Set the value of the inverse matrix
     setinv <- function(solve) m <<- solve 
     ## Get the value of the inverse matrix by calling m
     getinv <- function() m  
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
      ## first checks to see if the inverse matrix has already been calculated
     if(!is.null(m)){
       message("getting cached data")
       ## skips the computation
       return(m) 
     }
     ## Since the inverse matrix has not been calculated, calculate the 
     ## inverse matrix by calling solve() function.
     data <- x$get()
     m <- solve(data,...)
     x$setinv(m)
     m
}
