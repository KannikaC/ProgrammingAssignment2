makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(newValue) {
    x <<- newValue
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
