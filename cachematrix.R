# create an inverted matrix
# supply from cache if matrix exists

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize cache variable
  cache = NULL
  
  # create matrix
  set <- function(y)
  {
    x <<- y
    cache <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # invert matrix and put in cache
  setinv <- function(solve) cache <<- solve
  
  # retrieve matrix from cache
  getinv <- function() cache
  
  # return functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}




cacheSolve <- function(x, ...) {
  
  # get inverse of matrix if exists
  cache <- x$getinv()
  
  # check if matrix is cached and return
  if(!is.null(cache)) {
    message("this matrix is cached - retrieving")
    return(cache)
  }
  
  # create inverse matrix if not cached and return
  mdata <- x$get()
  cache <- solve(mdata, ...)
  x$setinv(cache)
  cache
        
}












