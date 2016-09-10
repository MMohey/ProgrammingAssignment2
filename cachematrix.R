
## Matrix inversion is often an expensive computational task. There could some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## The following pair of functions will cache the inverse of a matrix in 
## a special "matrix" object.



## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse.
## For this function, it will be assumed that the matrix given is always 
## invertible.

## This function, makeCacheMatrix creates a special 'matrix', which is 
## essentially a list containing a function to: set and get the value of the 
## matrix, set and get the value of the inverse matrix

makeCacheMatrix <- function(x_matrix = matrix()) {
  mat_inv <- NULL
  
  set <- function(new_mat){
    x_matrix <<- new_mat
    mat_inv <<- NULL
  }
  get <- function() x_matrix
  set_inverse <- function(mat_inverse) mat_inv <<- mat_inverse
  get_inverse <- function() mat_inv
  
  list(set = set, get = get, set_inverse = set_inverse, 
       get_inverse = get_inverse)
  ## returns the 'special matrix' containing all of the functions just 
  ##defined
}



## cacheSolve is a function that calculates the inverse of the special matrix 
## created with makeCacheMatrix function.

## First, it checks to see if the inverse matrix has been calculated, then it
## gets the matrix inverse from the cache and skips computations. Otherwise, it
## computes the matrix inverse of the data and sets its value via the 
## set_inverse() function

cacheSolve <- function(x_matrix, ...) {
  mat_inv <- x_matrix$get_inverse()
  
  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  
  data <- x_matrix$get()
  mat_inv <- solve(data,...)
  x_matrix$set_inverse(mat_inv)
  mat_inv
}
