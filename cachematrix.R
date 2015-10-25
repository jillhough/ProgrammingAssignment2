## makeCacheMatrix creates a special "matrix" type object
## that can cache its inverse. The inverse gets computed in the 
## function Cachesolve

## the function MakeCacheMatrix will create a special matrix
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set the value of the vector
  
  set <- function(y) {
    x <<- data.frame(y)
    m <<- NULL
  }
  
  # Get the value of the vector
  
  get <- function() x
  
  # Set the value of the mean
  
  setsolve <- function(solve) m <<- solve
  
  # Get the value of the mean
  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
