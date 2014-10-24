## These functions will cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Set the inverse
  i <- NULL
  
  ## Set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get and return matrix
  get <- function() {
    m
  }
  
  ## Set matrix inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get and return matrix inverse
  getInverse <- function() {
    i
  }
  
  ## Return methods list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute inverse of "makeCacheMatrix" 
cacheSolve <- function(x, ...) {
  
  ## Return inverse matrix
  m <- x$getInverse()
  
  ## Return inverse if set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix
  data <- x$get()
  
  ## Calculate inverse
  m <- solve(data) %*% data
  
  ## Set inverse
  x$setInverse(m)
  
  ## Return matrix
  m
}