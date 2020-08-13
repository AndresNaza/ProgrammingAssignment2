## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that stores a matrix, and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve uses the argument returned by the previous function in order to retrieve the inverse matrix that was stored previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}



#### Example 

matrix_example <- matrix(c(1,2,3,4),nrow=2,ncol=2)

makeCacheMatrix.object <- makeCacheMatrix(matrix_example)

cacheSolve(makeCacheMatrix.object)

cacheSolve(makeCacheMatrix.object)  #### Re-run and you'll see the legend "getting cached data" ;)
