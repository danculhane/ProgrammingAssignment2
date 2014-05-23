######
## Assignment 2 for R Programming
## Based on the example function makeCacheVector and cacheMean
## Daniel Culhane
## 05/22/2014
######

## These two functions, makeCacheMatrix and cacheSolve will save computation
## resources by caching the matrix inversion and only recalculating if the input 
## chnages.

## makeCacheMatrix will cache the inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #create a function that caches y to x and initializes m to null value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # assign a function with the arguement x to get
  get <- function() x
  # cache solve to a function that returns the inverse of a matrix m
  # and assign that matrix to setinverse
  setinverse <- function(solve) m <<- solve
  # assign a function with the matrix inverse to getinverse
  getinverse <- function() m
  #create list of new set, get, setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # test if m has been computed and display message
  # if the data is being retrieved
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m has not been computed and cached, assign x to data, using get()
  data <- x$get()
  # assign inverse of data to m, using the solve function.
  m <- solve(data, ...)
  # set inverse of x to m using the setinverse function
  x$setinverse(m)
  # return m
  m
}
