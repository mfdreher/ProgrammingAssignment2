## These fuctions take input values and stores them, and if available calculate the 
# inverse of a matrix using the solve function

## This function takes the input from a matrix and stores it

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function checks to see if the cache has a value. If it does it retuns the inverse of
## the matrix using the solve function

cacheSolve <- function(x, ...) {
  
  
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
    
  }
  
  data <- x$get()
  m <- solve(data, ...)    
  x$setinverse(m)
  
  m
}

