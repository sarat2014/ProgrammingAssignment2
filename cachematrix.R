# cachematrix.R will cache the inverse of a matrix
#
# Program Name : cachematrix.R
# Author Name  : Sarat Daggubati
# Date Created : 2014-07-17
# Assumption   : The matrix is invertible.
# Description  : This program cotains two functions
#                makeCacheMatrix : This function creates a special "matrix"
#                                  object that can cache its inverse.
#                cacheSolve      : This function computes the inverse of the
#                                  special "matrix" returned by makeCacheMatrix
#                                  above. If the inverse has already been
#                                  calculated (and the matrix has not changed),
#                                  then the cachesolve should retrieve the
#                                  inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  # makeCacheMatrix creates a special "matrix", which is really a list
  # containing a function to
  #    1. set the value of the matrix.
  #    2. get the value of the matrix.
  #    3. set the value of the inverse of the matrix.
  #    4. get the value of the inverse of the matrix.
  #
  # Args:
  #   x: An invertible matrix.
  #
  # Returns: A list for matrix object.
  
  
  # 0. Initialize an empty matrix as NULL
  inversedMatrix <- NULL
  
  # 1. set function will set the value of the matrix
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  
  # 2. get function will get the value of the matrix
  get <- function() x
  
  # 3. setMatrixInverse function will set the value of the inverse of the matrix
  setMatrixInverse <- function(inverse) inversedMatrix <<- inverse
  
  # 4. getMatrixInverse function will get the value of the inverse of the matrix
  getMatrixInverse <- function() inversedMatrix
  
  # list of the above 4 values
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

cacheSolve <- function(x, ...) {
  
  # The following function calculates the inverse of the special "matrix"
  # created with the above function. However, it first checks to see if
  # the inverse of the matrix has already been calculated. If so, it gets
  # the inverse of the matrix from the cache and skips the computation.
  # Otherwise, it calculates the inverse of the matrix and sets the value
  # of the inverse of the matrix in the cache via the solve() function.
  #
  # Args:
  #   x: An invertible matrix.
  #
  # Returns: Inversed matrix.
  
  
  # get the value of the inverse of the matrix from cache
  inversedMatrix <- x$getMatrixInverse()
  
  # check if the cache value exists
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    
    # if cache value exists, return the value of the inverse of the matrix
    return(inversedMatrix)
  }
  
  # cache value does not exist
  # get the value of the matrix
  givenMatrix <- x$get()
  
  # inverse the matrix using solve() function
  inversedMatrix <- solve(givenMatrix)
  
  # set the value of the inverse of the matrix
  x$setMatrixInverse(inversedMatrix)
  
  # return the value of the inverse of the matrix
  inversedMatrix
}
