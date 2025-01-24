## creates an object that caches matrices and their inverses

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  myinverse <- NULL
  set <- function(y) {
    x <<- y
    myinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) myinverse <<- solve
  getinverse <- function() myinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## if possible, returs the inverse of the special "matrix" from 
##  by makeCacheMatrix, else computes the inverse and stores it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  myinverse <- x$getinverse()
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  data <- x$get()
  myinverse <- solve(data, ...)
  x$setinverse(myinverse)
  myinverse
}
