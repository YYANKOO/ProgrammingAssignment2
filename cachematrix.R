## makeCacheMatrix creates and returns a matrix which is used
## by cacheSolve to get the inverse in cache

makeCacheMatrix <- function(x = matrix()) { 
  # initialize to NULL
  i <- NULL 
  # create the matrix in the working environment
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
  } 
  # get the value of the matrix
  get <- function() x 
  # invert the matrix and store in cache
  setinverse <- function(inv) i <<- inv 
  # get the inverted matrix from cache
  getinverse <- function() i 
  # return the created matrix to the working environment
  list( 
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse 
  ) 
} 


## cacheSolve calculates the inverse of the matrix created in 
## makeCacheMatrix. If the inverted matrix does not exist in cache, 
## then creates it in the working environment. If the inverted matrix 
## does exist, then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the inverse of matrix stored in cache
  i <- x$getinverse() 
  # if the inverse has been calculated, then get it from the cache
  # and skips the computation
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
  } 
  # calculate the inverse if it does not exist
  mat <- x$get() 
  i <- solve(mat, ...) 
  # set the value of the inverse in cache
  x$setinverse(i) 
  i 
} 

## TESTING
## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(m)
##           [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##           [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5



