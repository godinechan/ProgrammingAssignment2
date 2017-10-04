## This pair of functions computes the inverse of a matrix and cache it when complete,
## if a cache already exist it will pull that instead

## makeCacheMatrix creates a matrix and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve also computes the inverse of a matrix,
## but also check if a cache exist and pull that instead if it does

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}