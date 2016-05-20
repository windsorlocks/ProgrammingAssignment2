## stores an inverse of matrix in cache.
## provides functionality for storing and retrieving the cache
makeCacheMatrix = function(mtx = matrix()) {
  inverse <<- NULL;
  
  # write getter and setter for the cached matrix
  setMatrix = function(m) {
    mtx <<- m;
  }
  
  getMatrix = function() {
    return(mtx);
  }
  
  # getters and setters for inverse
  getMatrixInverse = function() {
    return(inverse);
  }
  
  setMatrixInverse = function(mi) {
    inverse <<- mi;
  }
  
  ##return everything
  list(set = setMatrix, get = getMatrix,
       getInverse = getMatrixInverse,
       setInverse = setMatrixInverse)
}


## returns a cached inverse of a matrix.
## In case where cached is not available, finds inverse, caches the inverse
## and then returns it
cacheSolve = function(matrix) {
  if(is.null(matrix)) {
    message("Matrix is null or not set. Try again")
    return(matrix);
  }

  #check to see if the inverse is cached
  inverse = matrix$getInverse();
  
  #if the inverse is cached return it istead of calculating afresh
  if(!is.null(inverse)) {
    message("Returning cached inverse of the matrix");
    return(inverse);
  }
  
  #call solve to inverse a non-singular matrix
  inverse = solve(matrix$get());
  
  #cache the calculated inverse 
  matrix$setInverse(inverse);
  
  return(inverse);
}
