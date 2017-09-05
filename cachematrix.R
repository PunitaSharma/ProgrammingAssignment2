## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##--------------------------------------------------

+## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
 +
 +makeCacheMatrix <- function(x = matrix()) {
 +
 +  i <- NULL
 +  set <- function(y) {
 +    
 +    x <<- y
 +    i <<- NULL
 +    
 +  }
 +  
 +  get <- function() x
 +  setInverse <- function(inverse) i <<- inverse
 +  getInverse <- function() i
 +  list(set = set,
 +       get = get,
 +       setInverse = setInverse,
 +       getInverse = getInverse)
 +  
 +}
 +
 +
 +## This second function, cacheSolve, computes the inverse of the special "matrix" created by 
 +## makeCacheMatrix above. The inverse has already been calculated (and the 
 +## matrix has not changed), so it should retrieve the inverse from the cache.
 +
 +cacheSolve <- function(x, ...) {
 +        ## Return a matrix that is the inverse of 'x'
 +  
 +  i <- x$getInverse()
 +  if (!is.null(i)) {
 +    message("getting cached data")
 +    return(i)
 +  }
 +  mat <- x$get()
 +  i <- solve(mat, ...)
 +  x$setInverse(i)
 +  i
 +}

##-------------------------------------------------

