## Goal: write a pair of functions that cache the inverse of a matrix
## makeCacheMatrix shoudl create a special "matrix" that can cache its inverse; cacheSolve shoudl compute the inverse of the matrix returned by makeCacheMatrix, if the inverse has been calculated then the cachesolve should retrive the inverse from the cache

## create matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <-function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## compute the inverse of matrix returned by makeCacheMatrix, will retrive inverse from cache if has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}
        ## Return a matrix that is the inverse of 'x'

