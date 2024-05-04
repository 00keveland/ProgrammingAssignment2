## Programming assignment 2:

## This code defines two functions, makeCacheMatrix and cacheSolve, for caching 
## the inverse of a matrix to optimize computation. makeCacheMatrix creates a 
## special matrix object capable of storing its inverse, while cacheSolve 
## computes and caches the inverse, retrieving it from the cache if it has 
## already been computed. These functions are tested with sample matrices to 
## demonstrate their functionality in computing and retrieving inverses efficiently.



## Function creating a special "matrix" object capable of caching its inverse.
makeCacheMatrix <- function() {
  # Initialize a variable to store the cached inverse matrix
  cache <- NULL
  
  # Function to set the matrix and compute its inverse
  set <- function(matrix) {
    # Assign the input matrix to the cache
    cache <<- matrix
    # Compute the inverse of the input matrix
    inverse <<- NULL  # Clear the previous inverse
    if (!is.null(matrix)) {
      inverse <<- solve(matrix)
    }
  }
  
  # Function to retrieve the cached inverse matrix
  get <- function() {
    # Return the cached inverse matrix
    inverse
  }
  
  # Return a list of functions
  #'set' function sets the matrix and computes its inverse, storing both in the cache
  #'get' function retrieves the cached inverse matrix

  list(set = set, get = get)
}


## Test that it functions correctly:
# Create a new cache matrix object
cache_matrix <- makeCacheMatrix()

# Set a test matrix
test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Set the test matrix using the set function
cache_matrix$set(test_matrix)

# Retrieve the cached inverse matrix using the get function
cached_inverse <- cache_matrix$get()

# Print the cached inverse matrix
print(cached_inverse)




## Function computes the inverse of a special "matrix" object returned by the 
## makeCacheMatrix function. If the inverse has already been calculated and 
## the matrix has not changed, the cacheSolve function retrieves the inverse 
## from the cache

cacheSolve <- function(cache_matrix) {
  # Check if the cache matrix has an inverse cached
  cached_inverse <- cache_matrix$get()
  if (!is.null(cached_inverse)) {
    message("Getting cached inverse matrix...")
    return(cached_inverse)  # Return the cached inverse
  } else {
    # If the inverse is not cached, compute it and cache it
    message("Computing inverse matrix...")
    matrix <- cache_matrix$set(NULL)  # Clear the previous inverse
    inverse <- solve(matrix)
    cache_matrix$set(matrix)
    return(inverse)
  }
}




## Now put it all together to test that both are functioning correctly

# Create a new cache matrix object
cache_matrix <- makeCacheMatrix()

# Set a test matrix
test_matrix <- matrix(c(4, 1, 1, 2), nrow = 2, ncol = 2)

# Compute the inverse using cacheSolve (should compute it)
inverse1 <- cacheSolve(cache_matrix)

# Print the computed inverse
print("Computed Inverse:")
print(inverse1)

# Retrieve the cached inverse using cacheSolve (should retrieve it from cache)
cached_inverse <- cacheSolve(cache_matrix)

# Print the retrieved cached inverse
print("Cached Inverse:")
print(cached_inverse)

# Change the test matrix
new_test_matrix <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

# Set the new test matrix
cache_matrix$set(new_test_matrix)

# Compute the inverse using cacheSolve (should recompute it)
inverse2 <- cacheSolve(cache_matrix)

# Print the recomputed inverse
print("Recomputed Inverse:")
print(inverse2)

