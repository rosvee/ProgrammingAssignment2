##functions that are able to cache potentially time-consuming computations of inverse of a matrix

##Part_1

##Function for creation of a speclial matrix that could cache its' inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Part_2

##Fuction calculates the inverse of the special matrix returned above. First checks if the inverse 
##has been calculated. If so, gets the inverse from cache, otherwise calculates it and sets the value in the cache through setinverse

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setmean(invs)
  invs
}