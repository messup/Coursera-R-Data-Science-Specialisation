# Creates a list of functions to access a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()){
  
  inverse_x <-  NULL
  
  set <- function(new_matrix){
    x <<- new_matrix
    inverse_x <<- NULL
  }
  
  get <- function(){
    x
  }
  
  set_inverse <- function(new_inverse){
    inverse_x <<- new_inverse
  }
  
  get_inverse <- function(){
    inverse_x
  }
  
  list(set = set,
       get = get,
       get_inverse = get_inverse,
       set_inverse = set_inverse)
}

# Expects a list produced by the MakeCacheMatrix function.
# Checks if the inverse has already been calculated.
# If not, it is calculated and added to the cache.
cacheSolve <- function(x, ...){
  
  inverse_x <- x$get_inverse()
  if(!is.null(inverse_x)){
    message("Getting cached data")
    return(inverse_x)
  }
  
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$set_inverse(inverse_x)
  inverse_x
}