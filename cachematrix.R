## kannan sundararaman -- coursera - R Programming 
## creating a matrix and caching the inverse value.
## cached value will be checked before calculating the inverse.
## if cached value exists it returns the value otherwise calculates, and cache it for reuse.
##########################################################
## example usage : 
##
##
##
##
##
##
##
##########################################################
## makeCacheMatrix has 4 methods get, set, getinverse, setinverse
##  getinverse is to get whether the inverse is already calculated and kept in cache
##  setinverse is to set the inverse which is effectively to be called by the cacheSolve function
##########################################################

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


## cacheSolve is the function which finds the inverse of the matrix and if its 
##  cached then it uses the cached value instead of calculating it.
##  cacheSolve expects the parameter having the getinverse method to check 
##  whether the cached value exists.  
##  though its an independant function, it require the setinverse method to cache the 
##  calculated inverse value.  The calculated inverse of matrix will use this method 
##  to cache the result.  
##  the parameter of cacheSolve expects getinverse, setinverse methods along with 
##  get method for picking the value of the matrix to be inversed.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
