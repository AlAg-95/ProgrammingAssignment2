## The following functions are focused on making more effiecent scripts where the inverse of a 
# matrix is evaluated repeatdely

## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse, 
# more specifically a list.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The second function cacheSolve computes the inverse of the special "matrix" `x` returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
