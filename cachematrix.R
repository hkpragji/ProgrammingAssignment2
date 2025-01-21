# Write a pair of functions that cache the inverse of a matrix
# To improve performance, we can cache the inverse rather than computing it repeatedly

# The makeCacheMatrix function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialise an empty variable to store the cached value of the inverse
  i <- NULL
  # Create a function to set the matrix
  set_matrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Create a function to get the matrix
  get_matrix <- function() {
    x
  }
  # Create a function to set the value of the matrix inverse
  set_inverse_value <- function(inverse) {
    i <<- inverse
  }
  # Create a function to get the value of the inverse i
  get_inverse_value <- function() {
    i
  }
  # Combine all the function elements into a list
  special <- list(
    set_matrix = set_matrix,
    get_matrix = get_matrix,
    set_inverse_value = set_inverse_value,
    get_inverse_value = get_inverse_value
  )
  # Return the list
  return(special)
}

# The cacheSolve function computes the inverse of the special matrix object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Check if the inverse has already been computed and stored in cache
  i <- x$get_inverse_value()
  # If i has already been calculated, retrieve the inverse
  if (!is.null(i)) {
    message("Retrieving the cached inverse.")
    return(i)
  }
  # If the inverse is null or the cached inverse was invalidated, get a matrix
  m <- x$get_matrix()
  # Calculate its inverse
  i <- solve(m, ...)
  # Store the computed inverse in cache
  x$set_inverse_value(i)
  # Return the computed inverse
  return(i)
}
