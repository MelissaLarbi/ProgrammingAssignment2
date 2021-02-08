## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  inv <- NULL
  set <- function(y) {  ## the function that change x 
    x <<- y           ## change x with y
    inv <<- NULL      ## let inv inchange
  }
  get <- function() x    ## get matrix  
  setinv <- function(inverse) inv <<- inverse  ## change the inverse of the matrix
  getinv <- function() inv  ## return the inverse 
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()  ## get the inverse of x 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()  ## get the matrix 
  m <- solve(mat, ...) ## get the inverse of the matrix
  x$setinv(m)         ## update the inverse 
  m
}







