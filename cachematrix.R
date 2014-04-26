## creates a matrix representation, and uses it cache matrix inversion operation results

## Function to create a matrix representation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCacheMatrix <- function(CacheMatrix) m <<- CacheMatrix
  getCacheMatrix <- function() m
  list(set = set, get = get,
       setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix)
}


## Computes the matrix inverse, and save it for future lookups to avoid recomputation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getCacheMatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCacheMatrix(m)
        m
}
