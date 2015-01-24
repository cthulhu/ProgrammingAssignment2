# Programming Assignment 2
#
# MOOC: Coursera RProg-010
# Student: Stanislav O. Pogrebnyak

makeCacheMatrix <- function(original_matrix = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  #
  # Args:
  #   orig.matrix: Matrix to cache
  #
  # Returns:
  #   list of functions to access original and inverted matrix

  inverted_matrix <- NULL
  
  # original matrix accessors
  set <- function(m) {
    original_matrix <<- m
    inverted_matrix <<- NULL
  }
  get <- function() original_matrix
  
  # inverted matrix accessors
  set_inverse <- function(i) inverted_matrix <<- i
  get_inverse <- function() inverted_matrix
  
  # list
  list(
    set = set, 
    get = get, 
    set_inverse = set_inverse, 
    get_inverse = get_inverse
  )
}

cacheSolve <- function( c_m, ... ) {
  # This function computes the inverse of the special "matrix" 
  # returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  #
  # Args:
  #   c_m: matrix to cache
  #
  # Returns:
  #   inverted matrix
  
  inverted_matrix <- c_m$get_inverse()
  
  if(!is.null(inverted_matrix)) {
    message("Getting cached result")
    return(inverted_matrix)
  }
  original_matrix = c_m$get()
  inverted_matrix = solve(original_matrix)
  c_m$set_inverse(inverted_matrix)
  inverted_matrix
}

