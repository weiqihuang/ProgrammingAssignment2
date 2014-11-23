## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCachematrix above. 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## If the inverse has already been calculated (and the matrix has 
  ## not changed), then the cacheSolve should retrieve the inverse 
  ## from the cache.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse has not been calculated, then the cacheSolve should
  ## retrieve the data, and compute and cache the inverse.
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}