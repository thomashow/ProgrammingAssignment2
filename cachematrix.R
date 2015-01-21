## Matrix inversion computation consume much processing power and time.
## This function is to perform Caching to allow repeat calling of matrix inversion on the same matrix to
## get the result from the Cache instead of recalculation

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize the cached inverse of matrix as NULL
  CachedInvMatrix <- NULL
  # when setting or assigning matrix y, we will keep the matrix in x
  set <- function(y) {
    x <<- y
    # as gthe matrix is assigned a new value, we shall clear the inverse matrix (set to NULL)
    CachedInvMatrix <<- NULL
  }
  # this is the get matrix function which is to return the x, the matrix
  get <- function() x
  
  # from external, when inverse matrix is computed using solve(), setInverse is to store the inverse into cache
  setInverse <- function(inversematrix) CachedInvMatrix <<- inversematrix
  
  # this function is to return the Cache Inverse Matrix
  getInverse <- function() CachedInvMatrix
  
  # return a list with the element as the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix x (a special vector created with makeCacheMatrix),
## calculate the inverse of matrix and store to the vector.  But if the inverse has been calculated before, then
## get it from the cache
cacheSolve <- function(x, ...) {
  # Get the inverse Matrix from cache
  m <- x$getInverse()
  
  # if not null, then m must be set with the inverse matrix from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if inverse matrix never been run before for 'x', then take the matrix  to assign to 'data', compute the inverse
  # of matrix and then store it back to the cache in 'x' 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  ## Return matrix 'm' that is the inverse of 'x'
  m
}
