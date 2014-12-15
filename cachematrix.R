## This is for RProg PA2
## The lexical scoping is used to cache the matrix inverse.
## The matrix inverse is lazily calculated. Once it is calculated,
## it will be cached for later access to improve the performance.
## When it is accessed later, the cached value is simply returned.


## makeCacheMatrix allows one to get/set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(set=set, get=get, 
       setinverse= setinverse, getinverse=getinverse)
}


## cacheSolve accesses the cached matrix inverse
## If it is cached before, it simply returns it;
## otherwise, the inverse is calculated and set to cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if (!is.null(invx)) {
    message("getting cached matrix inverse")
    return(invx)
  }
  data <- x$get()
  invx <- solve(x)
  x$setinverse(invx)
  invx
}
