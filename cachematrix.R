## This is the first function, where a matrix is assinged to the cache

makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## here using the set function, one can change the matrix that is in the cache
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve ## this function can define the inverse of a matrix, that is already calculated 
  getinverse <- function() m ## retrieves the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This is the second function, where the inverse of the matrix is assinged to the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { ## if the inverse is calculated previously then the console will print the mesage and return the inverse
    message("getting cached data")
    return(x$getinverse())
  }
  data <- x$get() ## if it isn't then the function will calculate it
  m <- solve(data)
  x$setinverse(m) ## this sets the inverse of the matrix that is in the cache, so as to not need to calculate it again
  x$getinverse() ## prints the calculation
}
