## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data, inverse has been already computed!")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

testCacheSolve <- function() {
  # random diagonal matrix with easy to compute inverse (has non zero entries in diag) 
  matrix_ <- makeCacheMatrix(matrix(diag(rpois(5,10)),ncol=5))
  # get the matrix itself
  matrix_$get()
  # get the inverse
  matrix_$getinverse()
  # compute the inverse, will not get into the if cond.
  print(cacheSolve(matrix_))
  
  print("new example")
  # Example that get into the if cond.
  matrix_$set(matrix(diag(5)))
  matrix_$setinverse(matrix(diag(5)))
  cacheSolve(matrix_)
}