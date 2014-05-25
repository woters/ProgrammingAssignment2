
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # setter
  set<- function(y) {x <<- y    i <<- NULL  }
  # getter
  get <- function() x
  #inverse
  setInv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  i <- x$getinv()  
  if (!is.null(i)) {return(i)}
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
