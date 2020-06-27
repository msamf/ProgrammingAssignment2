## These functions cache time-consuming computations, such as calculating the inverse of a matrix. 


## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(solve) inv <<- solve
  
  getInv <- function() inv
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the cache has already been calculated (and the matrix hasn't change), then
## cacheSolve should retrieve the inverse from the cache. 

## NOTE: assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getInv()
    if(!is.null(inv)) { # if it's cached, just retrieve
      message('getting cached data')
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv) # set inv after calculating 
    
    inv # return inv
}
