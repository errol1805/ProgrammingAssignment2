## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    if(nrow(y)!=ncol(y)) {
      message("please make sure matrix is square")

    }
    else {
      x <<- y
    }
      
    v <<- NULL
  }
  get <- function() x
  setinv <- function(inv) v <<- inv
  getinv <- function() v
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the specail "matrix" returned by makeCacheMatrix above. if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinv()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
