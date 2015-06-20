## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set new value of matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # get current value of matrix
  get <- function() x
  # set value of inverse
  setinverse <- function(inverse) m <<- inverse
  # get value of inverse
  getinverse <- function() m
  list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
