## create "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ## set the value of matrix
    m <<- NULL ## set the value of matrix m to NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve ## set inverse matrix
  getInverse<- function() m ## get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse= getInverse)
}



cacheSolve<- function(x, ...) {
  m <- x$getInverse()
  ## if matrix m is not NULL,then get cached m - inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## calculating inverse matrix
  x$setInverse(m) ## setting inverse matrix m to cache
  m
}  
