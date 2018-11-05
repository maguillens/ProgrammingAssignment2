## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#####################################################
# makeCacheMatrix: CREATE AN OBJECT THAT KEEPS IN THE CACHE THE INVERSE OF A MATRIX
#####################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
#####################################################
# cacheSolve: RETURNS THE INVERSE OF THE MATRIX STORED IN THE CACHE 
#             THAT WE STORE IN THE PREVIOUS FUNCTION
#####################################################
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Resultados del Caché")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

