# This function serves to cache the inverse of a matrix. Matrix inversion may take a long time to compute 
# for a large matrix, especially if it is being computed repeatedly. We can cache the inverse of the 
# matrix so that if we need it again, it can be looked up in the cache rather than computed again. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # sets a default if cacheSolve has not been used.
  set <- function(y){  # set: sets the value of the matrix.
    x <<- y  # caches the matrix to enable cachesolve to check if it has changed.
    inv <<- NULL  # sets inv to NULL (the matrix inverse if cachesolve was used). 
  }
  get<- function() x  # get: gets the value of the matrix.
  setinverse <- function(inverse) inv <<- inverse  # setinverse: sets the value of the inverse of the matrix.
  getinverse <- function () inv  # getinverse: gets the value of the inverse of the matrix.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # list houses the four functions.
}

# This function will calculate the inverse of the given matrix if it has not been previously calculated. 
# If the inverse has already been calculated, it gets it from the cache and skips the computation.

cachesolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'. Comparing the matrix to what was there before.
  inv <- x$getinverse()  # Gets the previously calculated inverse (if applicable).
  if (!is.null(inv)) {  # Has cachesolve been run before?
    return(inv)  # Gets the previously calculated inverse (if applicable).
  }  # Otherwise:
  mat <- x$get()  # Run the getmatrix function to get the values. 
  inv <- solve(mat, ...)  # Compute the value of the inverse matrix.
  x$setinverse(inv)  # Run the setinverse function on the inverse to cache the values.
  inv  # Returns the inverse matrix.
}