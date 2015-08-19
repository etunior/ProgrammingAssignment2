# This file exposes two functions.
# The first one is makeCacheMatrix, which encapsulate a matrix, giving it the
# ability to cache it's inverse.
# The second one is cacheSolve, which, given an input cacheMatrix (a matrix
# encapsulated by the makeCacheMatrix function), returns it's inverse, using
# the cached value as much as possible.

makeCacheMatrix <- function(targetMatrix = matrix()) {
  # Encapsulates a matrix, giving it the ability to cache it's inverse.
  # 
  # Args:
  #   targetMatrix: The matrix whose inverse must be returned. Default is an
  #                 empty matrix.
  #
  # Returns:
  #   Conceptually, a 'cacheMatrix', which is a list containing four functions
  #   allowing access to get and set both the underling matrix and it's inverse
  #   cached value.
  cachedInverseMatrix <- NULL
  setMatrix <- function(newMatrix) {
    # Sets the underling matrix of 'cacheMatrix' and resets it's inverse cache.
    # 
    # Args:
    #   newMatrix: the new matrix value.
    #
    # Returns:
    #   NULL
    targetMatrix <<- newMatrix
    cachedInverse <<- NULL
  }
  getMatrix <- function() {
    # Gets the underling function value.
    # 
    # Args:
    #   nothing
    #
    # Returns:
    #   The underling matrix.
    targetMatrix
  }
  setInverseMatrix <- function(inverseMatrix) {
    # Sets the inverse matrix cache.
    # 
    # Args:
    #   inverseMatrix: the inverse matrix.
    #
    # Returns:
    #   The inverse matrix.
    cachedInverseMatrix <<- inverse
  }
  getInverseMatrix <- function() {
    # Gets the cached inverse matrix.
    # 
    # Args:
    #   nothing
    #
    # Returns:
    #   The cached inverse matrix.
    cachedInverseMatrix
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(cacheMatrix, ...) {
  # Calculates the inverse of 'cacheMatrix', if there's not yet a cached one,
  # in which case, just returns it.
  # 
  # Args:
  #   cacheMatrix: The matrix whose inverse must be returned.
  #   ...: further arguments passed to the solve function
  #
  # Returns:
  #   A matrix that is the inverse of 'cacheMatrix'
  cachedInverseMatrix <- cacheMatrix$getInverseMatrix()
  if(!is.null(cachedInverseMatrix)) {
    message("getting cached inverse matrix")
    return(cachedInverseMatrix)
  }
  data <- cacheMatrix$getMatrix()
  newInverseMatrix <- solve(data, ...)
  cacheMatrix$setInverseMatrix(newInverseMatrix)
  newInverseMatrix
}