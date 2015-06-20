# makeCacheMatrix creates a matrix object that can cache inverse

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


# CacheSolve computes the inverse of the matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  # check if inverse already exits 
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()#get matrix object
  m <- solve(data,...) # calculate matrix inverse
  x$setinverse(m)
  m #return inverse
}

