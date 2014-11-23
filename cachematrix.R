## Put comments here that give an overall description of what your
## functions do

## Makes list containing helpers for caching 
makeCacheMatrix <- function(x = matrix()) {
  cachedResult <- NULL
  set <- function(newValue) {
    x <<- newValue
    cachedResult <<- NULL
  }
  
  get <- function() x
  setCached <- function(y) cachedResult <<- y
  getCached <- function() cachedResult
  list(set = set, get = get,
       setCached = setCached,
       getCached = getCached)    
}

## Returns inverse of a matrix (from cache, if it was calculated before)
cacheSolve <- function(x, ...) {
  result <- x$getCached()
  
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  
  data <- x$get()
  result <- solve(data, ...)
  x$setCached(result)
  result
}
