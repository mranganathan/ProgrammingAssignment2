## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix to feed to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize i
  i <- NULL
  
  # Assigns a matrix to x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Retrieves the value of x from the parent environment of makeCacheMatrix
  get <- function() x
  
  # Assign input argument to the value of i in the parent environment
  setinverse <- function(solve) i <<- solve
  
  # Retrieve inverse of matrix
  getinverse <- function() i
  
  # Create list of functions needed for cacheSolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # Retrieves an inverse from the object passed in as the argument 
  i <- x$getinverse()
  
  # If i is not NULL, we have a valid, cached inverse and can return it to the parent environment
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If inverse matrix is NULL, retrieve input argument of makeCacheMatrix function
  data <- x$get()
  
  # Calculate inverse of that matrix
  i <- solve(data, ...)
  
  # Set the inverse in the input object
  x$setinverse(i)
  
  # Returns the value of the inverse to the parent environment
  i
}
