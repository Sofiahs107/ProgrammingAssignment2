## The following pair of functions (a) makeCacheMatrix and (b) cacheSolve, addresses the creation of an invertible
## matrix and the ability to cache its inverse

## (a)makeCacheMatrix : A function to create an invertible matrix object 

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <- function(y) {                            ## Set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                             ## Get the value of the matrix
  setinverse <- function(solve) m <<- inverse     ## Set the inverse of the matrix
  getinverse <- function() m                      ## Get the value of the inversed matrix
  list(set = set, get = get,
       setinverse = setinverse
       getinverse = getinverse)
}


## (b)cacheSolve : A function that computes the inverse of the special matrix, from makeCacheMatrix, and 
## able to cache and retrieve the inverse of the matrix 

cacheSolve <- function(x, ...) {
  m <- $getinverse()                              ## From makeCacheMatrix, get the value of the inversed matrix
  if(!is.null(m)) {                               ## If inversed matrix is available,
    message ("getting cached data")               ## print message "getting cached data"
    return(m)
  }
  data <- x$get()                                 ## Else, get the matrix from makeCacheMatrix
  m <- solve(data,...)                            ## Calculate the inverse of the matrix
  x$setsolve(m)                                   ## Set the value of the inversed matrix in the cache
  m                                               ## Return a matrix that is the inverse of 'x'
  }        
