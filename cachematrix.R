## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).

## Useful references:
## - Transcript of Submitting Assignment to Github <https://class.coursera.org/rprog-013/forum/thread?thread_id=626>
## - Step by step to understand the instructions <https://class.coursera.org/rprog-013/forum/thread?thread_id=694>
## - Easy Inverse test <https://class.coursera.org/rprog-013/forum/thread?thread_id=127>

## This function creates a special "matrix" object that can cache its inverse. Following functions are defined:
##  set : create/update matrix
##  get : returns the matrix
##  setInverse : set the calculated inverse matrix
##  getInverse : returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # the inverse variable holds the matrix inverse (if calculated)
  inverse <- NULL
  
  # set : alter the matrix (and invalidate cached "inverse" object)
  set <- function(y) {
    # assign y to x
    x <<- y
    # invalidate cache
    inverse <- NULL
  }
  
  # get : returns the matrix
  get <- function() x
  
  # setInverse : sets the calculated inverse matrix
  setInverse <- function(newinverse) {
    inverse <<- newinverse
  }
  
  # getInverse : returns the cached inverse matrix if computed, otherwise it returns NULL
  getInverse <- function() inverse
  
  # function list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get cached value (might be NULL if not yet computed)
  inverse <- x$getInverse()
  
  if (is.null(inverse)) {
    # if inverse is not cached (== NULL), compute it
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    # update cached value
    x$setInverse(inverse)
    # return calculated value
  }
    return(inverse)
}