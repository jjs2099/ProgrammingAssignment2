# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y       # Assign the matrix to 'x'
    inv <<- NULL  # Reset cached inverse when matrix changes
  }
  
  # Get the value of the matrix
  get <- function() {
    return(x)
  }
  
  # Set the value of the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse
  getInverse <- function() {
    return(inv)
  }
  
  # Return a list of functions to interact with the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If cached inverse exists, return it with a message
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If cached inverse does not exist, compute it
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using 'solve'
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  return(inv)
}
